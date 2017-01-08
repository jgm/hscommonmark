module Main where

import CommonMark

import qualified Data.Text.IO as T
import CommonMark.Util
import CommonMark.Parser

main :: IO ()
main = T.getContents >>= showTree . parseCommonMark

