module Main where

import CommonMark

import CommonMark.Lexer
import qualified Data.Text.IO as T
import CommonMark.Util
import CommonMark.BlockParser

main :: IO ()
main = do
  inp <- T.getContents
  showTree $ parseBlocks $ tokenize inp

