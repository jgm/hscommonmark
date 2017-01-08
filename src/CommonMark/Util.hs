{-# LANGUAGE RankNTypes #-}
module CommonMark.Util ( addChild, showTree )
where

import CommonMark.Types
import Data.Tree.Zipper
import Data.Tree

-- and return the child
addChild :: Tree a -> TreePos Full a -> TreePos Full a
addChild new treepos =
  insert new (Data.Tree.Zipper.last (children treepos))

showTree :: forall a . (Show a) => Tree (Elt a) -> IO ()
showTree = putStrLn . drawTree .
  fmap (\(Elt tt _ ts) ->
    show tt ++ " " ++ show (tokensToText ts))

