module CommonMark.InlineParser ( parseInlines ) where
import CommonMark.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Control.Monad
import Control.Monad.RWS
import Data.Tree
import Data.Tree.Zipper
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.Parsec hiding (label)
import Text.Parsec.Pos (newPos)
import Data.Char (isAscii, isLetter, isSpace, isAlphaNum, isPunctuation)

import Debug.Trace

-- TODO:
-- [ ] check flankingness in canOpen, canClose
-- [ ] check div 3 rule
-- [ ] keep track in state of how far you've looked for openers
-- [ ] implement resolveLinksImages

traverseTreePos :: Monad m => (TreePos Full a -> m (TreePos Full a))
                -> TreePos Full a -> m (TreePos Full a)
traverseTreePos f tp = do
  tp' <- f tp
  tp'' <- case firstChild tp' of
               Nothing -> return tp'
               Just c  -> traverseTreePos f c
  case next tp'' of
       Just c  -> traverseTreePos f c
       Nothing -> case parent tp'' of
                       Just p  -> return p
                       Nothing -> return (root tp'')

impossible :: a
impossible = error "The impossible happened"

resolveLinksImages :: TreePos Full Inline -> InlineM (TreePos Full Inline)
resolveLinksImages
 = return -- TODO

resolveEmphasis :: TreePos Full Inline -> InlineM (TreePos Full Inline)
resolveEmphasis tp =
  if canCloseEmphasis tp
     then
       case findMatchingOpener tp of
            Nothing  -> return tp
            Just op  -> do
              let optoks = contentToks (label op)
              let cltoks = contentToks (label tp)
              let getTokInfo ts =
                   case ts of
                        [Token pos (TEmphChars _ _ n)] -> (n, pos)
                        _ -> impossible
              let (opchars, oppos) = getTokInfo optoks
              let (clchars, clpos) = getTokInfo cltoks
              let numchars = minimum [2, clchars, opchars]
              let elttype = case numchars of
                                 1 -> Emph
                                 2 -> Strong
                                 _ -> impossible
              let numchars = if elttype == Strong then 2 else 1
              let opdelims =
                   case optoks of
                        [Token (l,c) (TEmphChars ec o n)]
                          | n > numchars ->
                            [Token (l, c + n - numchars)
                              (TEmphChars ec o numchars)]
                          | otherwise -> optoks
                        _ -> impossible
              let cldelims =
                   case cltoks of
                        [Token (l,c) (TEmphChars ec o n)]
                          | n > numchars ->
                              [Token (l, c) (TEmphChars ec o numchars)]
                          | otherwise -> cltoks
                        _ -> impossible

              let (contentnodes, tp') = eatNodesBetween op tp
              let newnode = Node (Elt{ eltType = elttype
                                     , delimToks = opdelims ++ cldelims
                                     , contentToks = []}) contentnodes
              return $ insert newnode
                     $ shrinkRight numchars
                     $ shrinkLeft numchars
                     $ tp'

     else return tp

shrinkLeft :: Int -> TreePos Empty Inline -> TreePos Empty Inline
shrinkLeft numchars tp =
  case prevTree tp of
       Just t ->
         case contentToks (label t) of
              [Token (l, c) (TEmphChars ec o n)]
                | n > numchars -> nextSpace $ modifyLabel (\elt ->
                        elt{ contentToks =
                           [Token (l, c) (TEmphChars ec o (n - numchars))] }) t
                | otherwise -> delete t
              _ -> tp
       Nothing -> tp

shrinkRight :: Int -> TreePos Empty Inline -> TreePos Empty Inline
shrinkRight numchars tp =
  case nextTree tp of
       Just t ->
         case contentToks (label t) of
              [Token (l, c) (TEmphChars ec o n)]
                | n > numchars -> prevSpace $ modifyLabel (\elt ->
                          elt{ contentToks = [Token (l, c + numchars)
                                (TEmphChars ec o (n - numchars))] }) t
                | otherwise -> delete t
              _ -> tp
       Nothing -> tp

eatNodesBetween :: (Eq a, Show a)
                => TreePos Full a
                -> TreePos Full a
                -> ([Tree a], TreePos Empty a)
eatNodesBetween op cl = go ([], prevSpace cl)
  where go (accum, tp) =
          case prevTree tp of
               Just n
                  | label n == stoplabel -> (accum, tp)
                  | otherwise            -> go (tree n : accum, delete n)
               Nothing -> impossible
        stoplabel = label op

findMatchingOpener :: TreePos Full Inline -> Maybe (TreePos Full Inline)
findMatchingOpener tp = gofind (`canOpenFor` tp) tp
  where gofind pred tp =
         case prev tp of
                Just pr ->
                  if pred pr
                     then Just pr
                     else gofind pred pr
                Nothing -> Nothing

flankingness :: TreePos Full Inline -> (Bool, Bool)
flankingness tp =
  let nexttoks = (contentToks . label) <$> next tp
      prevtoks = (reverse . contentToks . label) <$> prev tp
      followedByUnicodeWhitespace = startsWithUnicodeWhitespace nexttoks
      followedByPunctuation = startsWithPunctuation nexttoks
      precededByUnicodeWhitespace = startsWithUnicodeWhitespace prevtoks
      precededByPunctuation = startsWithPunctuation prevtoks
      leftFlanking = not followedByUnicodeWhitespace &&
        (not followedByPunctuation ||
          (precededByUnicodeWhitespace || precededByPunctuation))
      rightFlanking = not precededByUnicodeWhitespace &&
        (not precededByPunctuation ||
          (followedByUnicodeWhitespace || followedByPunctuation))
  in  (leftFlanking, rightFlanking)

startsWithUnicodeWhitespace :: Maybe [Token] -> Bool
startsWithUnicodeWhitespace mbt =
  case mbt of
       Just (Token _ TSpace : _) -> True
       Just (Token _ TTab : _) -> True
       Just (Token _ (TEndline _) : _) -> True
       Just (Token _ (TSym c) : _) -> isSpace c
       Just _ -> False
       Nothing -> True

startsWithPunctuation :: Maybe [Token] -> Bool
startsWithPunctuation mbt =
  case mbt of
       Just (Token _ (TSym c) : _) -> isPunctuation c
       Just (Token _ (TBackticks _) : _) -> True
       Just (Token _ (TEmphChars _ _ _) : _) -> True
       Just (Token _ (TEscaped _) : _) -> True
       _ -> False


canOpenFor :: TreePos Full Inline -> TreePos Full Inline -> Bool
canOpenFor op cl =
  let (opLeftFlanking, opRightFlanking) = flankingness op
      (clLeftFlanking, clRightFlanking) = flankingness cl
  in case (contentToks (label cl), contentToks (label op)) of
       ([Token pos1 (TEmphChars ec1 o1 n1)],
        [Token pos2 (TEmphChars ec2 o2 n2)])   ->
          ec1 == ec2 &&
          opLeftFlanking && clRightFlanking
          && if clLeftFlanking || opRightFlanking
                then (o1 + o2) `mod` 3 /= 0
                else  True
        -- TODO diffs btw asterisk and underscore
       _ -> False

canCloseEmphasis :: TreePos Full Inline -> Bool
canCloseEmphasis tp =
  case label tp of
       Elt{ eltType = Txt
          , delimToks = dts
          , contentToks = [Token pos (TEmphChars ec o n)] } -> True
          -- TODO check all th econditions
       _ -> False

-- the idea here is that we'll start with startingTree,
-- and analyze the tokens in precedence order,
-- breaking them into subtrees and assigning delimToks
-- when appropriate.
parseInlines :: RefMap -> [Token] -> TreePos Full Inline
parseInlines refmap ts =
  fst $ evalRWS
          (  (tokensToNodes
          >=> return . toRootNode
          >=> traverseTreePos resolveLinksImages
          >=> traverseTreePos resolveEmphasis)
          ts)
          InlineConfig{ refMap = refmap }
          InlineState{ noGreaterThan = False }
     where toRootNode = fromTree .
                        (Node Elt{
                             eltType = Inlines
                           , delimToks = []
                           , contentToks = [] })

data InlineConfig = InlineConfig{
          refMap :: RefMap
        } deriving (Show)

data InlineState = InlineState{
          noGreaterThan :: Bool
        } deriving (Show)

type InlineM = RWS InlineConfig () InlineState

-- nogt = no greater than sign in remaining input
tokensToNodes :: [Token] -> InlineM [Tree Inline]
tokensToNodes [] = return []
tokensToNodes (t@(Token _ (TEndline _)) : ts) =
  (mknode Softbreak [] [t] :) <$> tokensToNodes ts
tokensToNodes (t@(Token _ (TSym '\\')) :
                     el@(Token _ (TEndline _)) : ts) =
  (mknode Linebreak [t] [el] :) <$> tokensToNodes ts
tokensToNodes (t@(Token _ TSpace) : ts) =
  case span isSpaceTok ts of
       (sps@(_:_:_), el@(Token _ (TEndline _)):rest) ->
            (mknode Linebreak [] (t:sps ++ [el]) :) <$> tokensToNodes rest
       _ -> (mknode Space [] [t] :) <$> tokensToNodes ts
    where isSpaceTok (Token _ TSpace) = True
          isSpaceTok _                = False
tokensToNodes (t@(Token pos (TBackticks n)) : ts) =
     case break (\(Token _ ty) -> ty == TBackticks n) (adjustBackticks ts) of
           (codetoks, (endbackticks:rest)) ->
                (mknode Code [t, endbackticks] (undoEscapes codetoks) :) <$>
                tokensToNodes rest
           _ -> (mknode Txt [] [t] :) <$> tokensToNodes ts
      where adjustBackticks :: [Token] -> [Token]
            adjustBackticks [] = []
            adjustBackticks (Token pos (TEscaped '`') :
                             Token (l,c) (TBackticks m) : ts)
              | m == n - 1 = Token pos (TSym '\\') :
                             Token (l, c - 1) (TBackticks n) : ts
            adjustBackticks (t:ts) = t : adjustBackticks ts
tokensToNodes (t@(Token pos (TSym '<')) : ts) = do
  nogt <- gets noGreaterThan
  if nogt
     then (mknode Txt [] [t] :) <$> tokensToNodes ts
     else
      case break (\(Token _ ty) -> ty == TSym '>') ts of
         (tagbody, (gt:rest)) ->
           let bodytoks = undoEscapes tagbody
               tagtoks = t : bodytoks++ [gt]
           in ((case parseAutolink tagtoks of
                 Just lnk -> mknode Link{ linkDestination = lnk
                                        , linkTitle =  mempty } [t, gt]
                                    bodytoks
                 Nothing ->
                   case parseTags (mconcat (map tokenToText tagtoks)) of
                      (TagOpen _ _:_) -> mknode HtmlInline [] tagtoks
                      (TagClose _:_) -> mknode HtmlInline [] tagtoks
                      (TagComment _:_) -> mknode HtmlInline [] tagtoks
                      _ -> mknode Txt [] [t]) :) <$> tokensToNodes rest
         _ -> (mknode Txt [] [t] :) <$>
                  (do modify (\st -> st{ noGreaterThan = True })
                      tokensToNodes ts)
tokensToNodes (t:ts) =
  (mknode Txt [] [t] :) <$> tokensToNodes ts

undoEscapes :: [Token] -> [Token]
undoEscapes = map go
  where go (Token pos (TEscaped c)) = Token pos (TStr (Text.pack ['\\',c]))
        go t = t

mknode :: InlineType -> [Token] -> [Token] -> Tree Inline
mknode ty ds ts =
  Node Elt{ eltType = ty
          , delimToks = ds
          , contentToks = ts} []

-- finds first opening bracket to the left
findOpeningBracket :: TreePos Full Inline -> Maybe (TreePos Full Inline)
findOpeningBracket treepos =
  case prev treepos of
        Nothing   -> Nothing
        Just tp   -> case contentToks (label tp) of
                          [Token _ (TSym '[')] -> Just tp
                          _ -> findOpeningBracket tp

-- parsec parsers

parseAutolink :: [Token] -> Maybe Text
parseAutolink ts =
  case parse pAutolink "source" ts of
       Left _ -> Nothing
       Right r -> Just r

type Parser = Parsec [Token] ()

pSat :: (Text -> Bool) -> Parser Text
pSat pred =
  token (Text.unpack . tokenToText) (\(Token (l,c) _) -> newPos "source" l c)
     (\tok -> case tokenToText tok of
                   s | pred s -> Just s
                     | otherwise -> Nothing)

pSatisfy :: (Token -> Bool) -> Parser Token
pSatisfy pred =
  token (Text.unpack . tokenToText) (\(Token (l,c) _) -> newPos "source" l c)
     (\tok -> if pred tok then Just tok else Nothing)

pSym :: Char -> Parser Token
pSym c =
  pSatisfy $ \tok -> case tok of
                          Token _ (TSym d) | d == c -> True
                          _ -> False

pScheme :: Parser Text
pScheme = do
  x <- pSat $ \s -> Text.all (\c -> isAscii c && isAlphaNum c) s &&
                         not (Text.null s) &&
                         isLetter (Text.head s)
  xs <- many $ pSat $ \s -> Text.all (\c -> isAscii c &&
                                 (isAlphaNum c || c `elem` ['.','-','+'])) s
  let scheme = mconcat (x:xs)
  let slen = Text.length scheme
  guard $ slen >= 2 && slen <= 32
  return scheme

pAbsoluteURI :: Parser Text
pAbsoluteURI = do
  sch <- pScheme
  pSym ':'
  rest <- many (pSat (\s -> not (Text.any isSpace s) &&
                            not (Text.any (=='<') s) &&
                            not (Text.any (=='>') s)))
  return (sch <> Text.pack ":" <> mconcat rest)

pEmail :: Parser Text
pEmail = do
  mzero -- TODO
  return mempty

pAutolink :: Parser Text
pAutolink = do
  pSym '<'
  res <- pAbsoluteURI <|> pEmail
  pSym '>'
  eof
  return res

