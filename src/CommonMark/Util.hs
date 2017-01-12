{-# LANGUAGE RankNTypes #-}
module CommonMark.Util ( addChild, showTree )
where

import CommonMark.Types
import CommonMark.Zipper
import Data.Tree

-- and return the child
addChild :: Tree a -> TreePos Full a -> TreePos Full a
addChild new treepos =
  insert new (CommonMark.Zipper.last (children treepos))

showTree :: forall a . (Show a) => Tree (Elt a) -> IO ()
showTree = putStrLn . drawTree .
  fmap (\(Elt tt _ ts) ->
    (unwords $ take 1 $ words $ show tt) ++ " " ++ show (tokensToText ts))

