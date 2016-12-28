module CommonMark.BlockParser (
    BlockTree
  , Block(..)
  , BlockType(..)
  , gobbleSpaces
  , parseBlocks
  , parseLine
  , analyzeLine
)
where
import CommonMark.Types (Line, Token(..), Tok(..), BlockTree,
                         Block(..), BlockType(..))
import CommonMark.Lexer (tokenize)
import Control.Monad
import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.Split (split, keepDelimsR, whenElt)
import Debug.Trace

splitLines :: [Token] -> [Line]
splitLines = split (keepDelimsR (whenElt isNewline))
              where isNewline (Token _ (TEndline _)) = True
                    isNewline _                      = False

parseBlocks :: [Token] -> Tree Block
parseBlocks = toTree . foldr parseLine (fromTree emptyDoc) . splitLines

parseLine :: Line -> BlockTree -> BlockTree
parseLine line treepos =
  case analyzeLine (reverse (ancestors treepos)) line of
       Nothing  -> error "analyzeLine returned Nothing" -- shouldn't happen
       Just (lastMatched, rest) ->
         let (tip, rest') = if isContainerBlock lastMatched
                               then openNewBlocks lastMatched rest
                               else (lastMatched, rest)
         in  addTokens tip rest'

isContainerBlock :: BlockTree -> Bool
isContainerBlock bt = case blockType (rootLabel (toTree bt)) of
                      Document   -> True
                      BlockQuote -> True
                      List       -> True
                      Item       -> True
                      _          -> False

acceptsTokens :: BlockTree -> Bool
acceptsTokens bt = case blockType (rootLabel (toTree bt)) of
                   Paragraph   -> True
                   Heading     -> True
                   CodeBlock{} -> True
                   HtmlBlock   -> True
                   _           -> False

-- We can assume that the treepos is a container block.
openNewBlocks :: BlockTree -> Line -> (BlockTree, Line)
openNewBlocks treepos line =
  case matchNewBlock line of
       Just (delim, rest, newblock) ->
         let newtip = addChild newblock treepos
         in openNewBlocks newtip rest
       Nothing -> (treepos, line)

addChild :: Tree Block -> BlockTree -> BlockTree
addChild newblock treepos = insert newblock (children treepos)

addTokens :: BlockTree -> Line -> BlockTree
addTokens treepos line =
  if acceptsTokens treepos
     then addContentToks line treepos
     else
       if isBlank line
          then treepos
          else addContentToks line $
                 maybe (addChild emptyParagraph treepos)
                   id (findOpenParagraph treepos)
  where addContentToks ts = modifyLabel (\label ->
               label{ contentToks = contentToks label ++ ts })

-- find a Paragraph that is the descendent of treepos by lastChild.
findOpenParagraph :: BlockTree -> Maybe BlockTree
findOpenParagraph treepos
  | blockType (rootLabel (toTree treepos)) == Paragraph =
    Just treepos
  | otherwise =
    case (lastChild treepos) of
         Just child -> findOpenParagraph child
         Nothing    -> Nothing

matchNewBlock :: [Token] -> Maybe ([Token], [Token], Tree Block)
matchNewBlock ts =
  case gobbleSpaces 4 ts of
       Just rest -> return ([], rest, emptyCodeBlock)
       Nothing   ->
         case dropWhile isSpaceToken ts of
              (t@(Token pos TGreaterThan) : xs) -> do
                 let delim = maybe [t] (t:) (gobbleSpaces 1 xs)
                 let rest' = drop (length delim - 1) xs
                 return (delim, rest', emptyBlockQuote)
              _ -> mzero

isSpaceToken :: Token -> Bool
isSpaceToken (Token _ TSpace) = True
isSpaceToken (Token _ TTab)   = True
isSpaceToken _                = False

emptyDoc :: Tree Block
emptyDoc = Node (Block Document [] []) []

emptyCodeBlock :: Tree Block
emptyCodeBlock = Node (Block CodeBlock{ codeIndented = True
                                      , codeInfoString = Text.pack ""
                                      } [] []) []

emptyBlockQuote :: Tree Block
emptyBlockQuote = Node (Block BlockQuote [] []) []

emptyParagraph :: Tree Block
emptyParagraph = Node (Block Paragraph [] []) []

-- | Analyze line and return treepos of last matched
-- container node plus the remainder of the line,
-- after matched delimiters.
analyzeLine :: [BlockTree] -> Line -> Maybe (BlockTree, Line)
analyzeLine [] line = Nothing
analyzeLine (n:ns) line =
  case matchMarker n line of
       Nothing          -> Nothing
       Just (ds, line') -> Just $ go ns (addDelims ds n, line')
  where go []       (m, l) = (m, l)
        go (k:rest) (m, l) = case matchMarker k l of
                                  Nothing        -> (m, l)
                                  Just (ds', l') -> go rest
                                                    (addDelims ds' k, l')

addDelims :: [Token] -> BlockTree -> BlockTree
addDelims [] treepos = treepos
addDelims ds treepos = modifyLabel
  (\label -> label{ delimToks = delimToks label ++ ds }) treepos

matchMarker :: BlockTree -> [Token] -> Maybe ([Token], [Token])
matchMarker treepos line = do
  let tree' = tree treepos
      block = rootLabel tree'
  case blockType block of
       Document      -> return ([], line)
       BlockQuote    -> removeBlockQuoteStart line
       List          -> return ([], line)
       Item          -> undefined  -- TODO
       Paragraph     -> do
         guard (not (isBlank line))
         return ([], line)
       Heading       -> mzero
       CodeBlock { codeIndented = indented }
         | indented  -> case gobbleSpaces 4 line of
                             Nothing  -> mzero
                             Just line' -> return ([], line')
         | otherwise -> return ([], line)
       HtmlBlock     -> undefined  -- TODO
       ThematicBreak -> mzero

gobbleSpaces :: Int -> [Token] -> Maybe [Token]
gobbleSpaces 0 ts = Just ts
gobbleSpaces n (Token _ TSpace : xs) =
  gobbleSpaces (n - 1) xs
gobbleSpaces n (Token (ln,col) TTab : xs) =
  case 4 - (col `mod` 4) of
       remaining | remaining < n  -> gobbleSpaces (n - remaining) xs
                 | remaining == n -> Just xs
                 | remaining > n  -> gobbleSpaces n (newspaces ++ xs)
                     where newspaces = map (\c -> Token (ln,c) TSpace)
                                        [col..(col + remaining - 1)]
gobbleSpaces n _ = Nothing

skipSpaces :: [Token] -> [Token]
skipSpaces (Token _ TSpace : xs) = skipSpaces xs
skipSpaces (Token _ TTab : xs) = skipSpaces xs
skipSpaces xs = xs

removeBlockQuoteStart :: [Token] -> Maybe ([Token], [Token])
removeBlockQuoteStart ts = removeOneLeadingSpace <$>
  case ts of
       (Token p1 TGreaterThan : xs) ->
          return ([Token p1 TGreaterThan], xs)
       (Token p1 TSpace : Token p2 TGreaterThan : xs) ->
          return ([Token p2 TGreaterThan], xs)
       _ -> mzero

removeOneLeadingSpace :: ([Token], [Token]) -> ([Token], [Token])
removeOneLeadingSpace (ts, Token (l,c) TSpace : xs) = (ts, xs)
removeOneLeadingSpace (ts, Token (l,c) TTab : xs) =
  let remaining = 4 - (c `mod` 4)
  in  if remaining > 1
         then (ts ++ [Token (l,c) TSpace],
               map (\n -> Token (l, c+n) TSpace) [1..(remaining - 1)] ++ xs)
         else (ts, xs)
removeOneLeadingSpace (ts, xs) = (ts, xs)

isBlank :: Line -> Bool
isBlank toks = all isBlankTok toks

isBlankTok :: Token -> Bool
isBlankTok (Token _ t) =
  case t of
       TSpace -> True
       TTab   -> True
       _      -> False

ancestors :: BlockTree -> [BlockTree]
ancestors treepos =
  case parent treepos of
        Nothing   -> [treepos]
        Just tp   -> tp : ancestors tp


