module CommonMark.InlineParser ( parseInlines ) where
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
                                              _                    -> Txt
                              , delimToks = []
                              , contentToks = [t]} []


