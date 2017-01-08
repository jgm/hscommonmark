module Main where

import CommonMark

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import CommonMark.Util
import CommonMark.Parser
import CommonMark.Render.Html5

main :: IO ()
main = T.getContents >>= TL.putStrLn . renderHtml5 . parseCommonMark

