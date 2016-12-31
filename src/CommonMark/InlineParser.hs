module CommonMark.InlineParser ( parseInlines ) where
import CommonMark.Types
import qualified Data.Text as Text
import Data.Tree
import Data.Tree.Zipper
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Text.HTML.TagSoup (Tag(..), parseTags)

-- TODO
-- [ ] reference map param to parseInlines?
-- [ ] autolinks
-- [ ] note that we'll have to run undoEscapes on contents of
--     raw HTML, autolinks, <url> in links since escapes don't
--     function in these contexts (unless we change that for <url>).
-- [ ] POSTPROCESSING: links and images
-- [ ] POSTPROCESSING: emphasis and strong

-- the idea here is that we'll start with startingTree,
-- and analyze the tokens in precedence order,
-- breaking them into subtrees and assigning delimToks
-- when appropriate.
parseInlines :: [Token] -> TreePos Full Inline
parseInlines ts = fromTree (Node Elt{ eltType = Inlines
                                    , delimToks = []
                                    , contentToks = []}
                                  (tokensToNodes False ts))

-- nogt = no greater than sign in remaining input
tokensToNodes :: Bool -> [Token] -> [Tree Inline]
tokensToNodes _ [] = []
tokensToNodes nogt (t@(Token _ (TEndline _)) : ts) =
  mknode Softbreak [] [t] : tokensToNodes nogt ts
tokensToNodes nogt (t@(Token _ (TSym '\\')) :
                     el@(Token _ (TEndline _)) : ts) =
  mknode Linebreak [t] [el] : tokensToNodes nogt ts
tokensToNodes nogt (t@(Token _ TSpace) : ts) =
  case span isSpaceTok ts of
       (sps@(_:_:_), el@(Token _ (TEndline _)):rest) ->
            mknode Linebreak [] (t:sps ++ [el]) : tokensToNodes nogt rest
       _ -> mknode Space [] [t] : tokensToNodes nogt ts
    where isSpaceTok (Token _ TSpace) = True
          isSpaceTok _                = False
tokensToNodes nogt (t@(Token pos (TBackticks n)) : ts) =
     case break (\(Token _ ty) -> ty == TBackticks n) (adjustBackticks ts) of
           (codetoks, (endbackticks:rest)) ->
                mknode Code [t, endbackticks] (undoEscapes codetoks)
                : tokensToNodes nogt rest
           _ -> mknode Txt [] [t] : tokensToNodes nogt ts
      where adjustBackticks :: [Token] -> [Token]
            adjustBackticks [] = []
            adjustBackticks (Token pos (TEscaped '`') :
                             Token (l,c) (TBackticks m) : ts)
              | m == n - 1 = Token pos (TSym '\\') :
                             Token (l, c - 1) (TBackticks n) : ts
            adjustBackticks (t:ts) = t : adjustBackticks ts
tokensToNodes nogt (t@(Token pos (TSym '<')) : ts) =
  case break (\(Token _ ty) -> ty == TSym '>') ts of
       (tagtoks, (gt:rest)) ->
         (case parseTags (mconcat (map tokenToText (t : tagtoks ++ [gt]))) of
              (TagOpen _ _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              (TagClose _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              (TagComment _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              _ -> mknode Txt [] [t]) : tokensToNodes nogt rest
       _ -> mknode Txt [] [t] : tokensToNodes True ts
tokensToNodes nogt (t:ts) =
  mknode Txt [] [t] : tokensToNodes nogt ts

undoEscapes :: [Token] -> [Token]
undoEscapes = map go
  where go (Token pos (TEscaped c)) = Token pos (TStr (Text.pack ['\\',c]))
        go t = t

mknode :: InlineType -> [Token] -> [Token] -> Tree Inline
mknode ty ds ts =
  Node Elt{ eltType = ty
          , delimToks = ds
          , contentToks = ts} []

-- iterate from startpos to endpos, deleting each
-- node and adding its contentsTok to contentsTok of
-- a new code node -- (except for startpos and endpos, which
-- go to delimsTok).  Then insert the new code node.
-- and return the paren

canOpen :: TreePos Full Inline -> Bool
canOpen treepos = undefined

canClose :: TreePos Full Inline -> Bool
canClose treepos = undefined

-- finds first closing delimiter run to the right of treepos
findClosingDelimiterRun :: TreePos Full Inline
                        -> Maybe (TreePos Full Inline)
findClosingDelimiterRun treepos =
  case next treepos of
       Just tp | canClose tp -> Just tp
               | otherwise  -> findClosingDelimiterRun tp
       Nothing  -> Nothing

-- finds first matching opening delimiter run to the left of a closing
-- run
findOpeningDelimiterRun :: TreePos Full Inline
                        -> Maybe (TreePos Full Inline)
findOpeningDelimiterRun treepos =
  case contentToks (label treepos) of
       [Token _ (TAsterisks n)]   -> go (TAsterisks n)   treepos
       [Token _ (TUnderscores n)] -> go (TUnderscores n) treepos
       _ -> Nothing
  where go :: Tok -> TreePos Full Inline -> Maybe (TreePos Full Inline)
        go tt treepos =
          case prev treepos of
                Just tp | canOpen tp  ->
                  case (tt, contentToks (label tp)) of
                       (TAsterisks n, [Token _ (TAsterisks m)]) -> Just tp
                       (TUnderscores n, [Token _ (TUnderscores m)]) -> Just tp
                       _ -> findOpeningDelimiterRun tp
                        | otherwise -> findOpeningDelimiterRun tp
                Nothing   -> Nothing

-- finds first opening bracket to the left
findOpeningBracket :: TreePos Full Inline -> Maybe (TreePos Full Inline)
findOpeningBracket treepos =
  case prev treepos of
        Nothing   -> Nothing
        Just tp   -> case contentToks (label tp) of
                          [Token _ (TSym '[')] -> Just tp
                          _ -> findOpeningBracket tp
