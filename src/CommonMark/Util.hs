{-# LANGUAGE RankNTypes #-}
module CommonMark.Util ( addChild, showTreePos )
where

import CommonMark.Types
import Data.Tree.Zipper
import Data.Tree

-- and return the child
addChild :: Tree a -> TreePos Full a -> TreePos Full a
addChild new treepos =
  insert new (Data.Tree.Zipper.last (children treepos))

showTreePos :: forall a . (Show a) => TreePos Full (Elt a) -> IO ()
showTreePos = putStrLn . drawTree .
  fmap (\(Elt tt _ ts) ->
    show tt ++ " " ++ show (mconcat (map tokenToText ts))) . toTree

