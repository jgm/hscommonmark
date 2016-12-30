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
                                  (tokensToNodes False (mkBacktickMap ts) ts))

-- the backtick map is an optimization that allows us to be avoid
-- scanning unnecessarily for closing backtick spans.
type BacktickMap = IntMap.IntMap Pos

-- This is to avoid performance problems with things like
-- ` `` ``` ```` ```` ... we can tell instantly if there is
-- a backtick span of a certain length after a token position.
-- Unfortunately it imposes a small fixed cost on all inputs.
mkBacktickMap :: [Token] -> BacktickMap
mkBacktickMap = foldl' f mempty
  where f tickmap (Token pos (TBackticks n)) = IntMap.insert n pos tickmap
        f tickmap _ = tickmap

-- nogt = no greater than sign in remaining input
tokensToNodes :: Bool -> BacktickMap -> [Token] -> [Tree Inline]
tokensToNodes _ _ [] = []
tokensToNodes nogt tickmap (bs@(Token _ (TSym '\\')) : t@(Token (l,c) ty) : ts)
  = case ty of
       TEndline _ -> mknode Linebreak [bs] [t] : tokensToNodes nogt tickmap ts
       TBackticks n -> mknode Txt [bs] [Token (l,c) (TSym '`')] :
                        tokensToNodes nogt tickmap
                          (if n > 1
                              then ((Token (l,c+1) (TBackticks (n-1))) : ts)
                              else ts)
       TAsterisks n -> mknode Txt [bs] [Token (l,c) (TSym '*')] :
                        tokensToNodes nogt tickmap
                          (if n > 1
                              then ((Token (l,c+1) (TAsterisks (n-1))) : ts)
                              else ts)
       TUnderscores n -> mknode Txt [bs] [Token (l,c) (TSym '_')] :
                        tokensToNodes nogt tickmap
                          (if n > 1
                              then ((Token (l,c+1) (TUnderscores (n-1))) : ts)
                              else ts)
       TSym c -> mknode Txt [bs] [t] : tokensToNodes nogt tickmap ts
       _ -> mknode Txt [] [bs] : tokensToNodes nogt tickmap (t:ts)
tokensToNodes nogt tickmap (t@(Token _ (TEndline _)) : ts) =
  mknode Softbreak [] [t] : tokensToNodes nogt tickmap ts
tokensToNodes nogt tickmap (t@(Token _ TSpace) : ts) =
  mknode Space [] [t] : tokensToNodes nogt tickmap ts
tokensToNodes nogt tickmap (t@(Token pos (TBackticks n)) : ts) =
  case IntMap.lookup n tickmap of
       Just pos' | pos' > pos ->
         case break (\(Token _ ty) -> ty == TBackticks n) ts of
              (codetoks, (endbackticks:rest)) ->
                 mknode Code [t, endbackticks] codetoks
                 : tokensToNodes nogt tickmap rest
              _ -> mknode Txt [] [t] : tokensToNodes nogt tickmap ts
       _ -> mknode Txt [] [t] : tokensToNodes nogt tickmap ts
tokensToNodes nogt tickmap (t@(Token pos (TSym '<')) : ts) =
  case break (\(Token _ ty) -> ty == TSym '>') ts of
       (tagtoks, (gt:rest)) ->
         (case parseTags (mconcat (map tokenToText (t : tagtoks ++ [gt]))) of
              (TagOpen _ _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              (TagClose _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              (TagComment _:_) -> mknode HtmlInline [] (t : tagtoks ++ [gt])
              _ -> mknode Txt [] [t]) : tokensToNodes nogt tickmap rest
       _ -> mknode Txt [] [t] : tokensToNodes True tickmap ts
tokensToNodes nogt tickmap (t:ts) =
  mknode Txt [] [t] : tokensToNodes nogt tickmap ts

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
