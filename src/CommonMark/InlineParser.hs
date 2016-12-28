module CommonMark.InlineParser ( parseInlines, findClosingBackticks ) where
import CommonMark.Types
import Data.Tree
import Data.Tree.Zipper

-- the idea here is that we'll start with startingTree,
-- and analyze the tokens in precedence order,
-- breaking them into subtrees and assigning delimToks
-- when appropriate.  eventually we'll need to consolidate
-- adjacent Txt nodes too.
parseInlines :: [Token] -> TreePos Full Inline
parseInlines ts = startingTree
  where startingTree = fromTree (Node Elt{ eltType = Inlines
                                         , delimToks = []
                                         , contentToks = []}
                                  (map tokToNode ts))
        tokToNode t = Node Elt{ eltType = case t of
                                              Token _ (TEndline _) -> Softbreak
                                              Token _ (TSpace)     -> Space
                                              _                    -> Txt
                              , delimToks = []
                              , contentToks = [t]} []

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

-- finds first closing backtick span of given size to the right of treepos
findClosingBackticks :: Int -> TreePos Full Inline
                     -> Maybe (TreePos Full Inline)
findClosingBackticks n treepos =
  case next treepos of
        Nothing   -> Nothing
        Just tp   -> case contentToks (label tp) of
                          [Token _ (TBackticks n)] -> Just tp
                          _ -> findClosingBackticks n tp
