module Main where

import CommonMark

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as T
import CommonMark.Util
import CommonMark.Parser
import CommonMark.Render.Html5
import System.Environment
import Data.List (intersperse)

main :: IO ()
main = do
  args <- getArgs
  inp <- case args of
              [] -> T.getContents
              xs -> (mconcat . intersperse (T.singleton '\n'))
                            <$> mapM T.readFile xs
  let parsed = parseCommonMark inp
  showTree parsed
  TL.putStrLn $ renderHtml5 parsed

