module Main where

import CommonMark

import CommonMark.Lexer (tokenize)
import qualified Data.Text.IO as T

main :: IO ()
main =
  T.getContents >>= print . tokenize

