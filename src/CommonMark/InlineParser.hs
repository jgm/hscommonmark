module CommonMark.InlineParser ( parseInlines ) where
import CommonMark.Types
import Data.Tree
import Data.Tree.Zipper
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Text.HTML.TagSoup (Tag(..), parseTags)

-- TODO
-- [ ] reference map param to parseInlines?
-- [x] resolve escapes
-- [x] inline HTML
-- [ ] entities (should be recognized by tokenizer?)
-- [ ] autolinks
-- [ ] handle two-space hard line breaks
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
-- Note: we have to do backslash escaping here; the tokenizer
-- doesn't have enough information, since it doesn't know whether
-- \` occurs within a code span.
-- TODO: do backslash-escaping in the lexer, and handle the above
-- problem with a special case here?  that might be simpler.
-- if this is the ONLY special case to worry about... (what
-- about autolinks, <url> in inline and reference links,
-- and attribute contents in raw HTML?  well, we can always
-- systematically reconvert the Escape tokens in these cases.)
tokensToNodes :: Bool -> [Token] -> [Tree Inline]
tokensToNodes _ [] = []
tokensToNodes nogt (t@(Token _ (TEndline _)) : ts) =
  mknode Softbreak [] [t] : tokensToNodes nogt ts
tokensToNodes nogt (t@(Token _ TSpace) : ts) =
  mknode Space [] [t] : tokensToNodes nogt ts
tokensToNodes nogt (t@(Token pos (TBackticks n)) : ts) =
     case break (\(Token _ ty) -> ty == TBackticks n) (adjustBackticks ts) of
           (codetoks, (endbackticks:rest)) ->
                mknode Code [t, endbackticks] codetoks
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
