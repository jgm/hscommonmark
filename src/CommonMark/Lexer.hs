module CommonMark.Lexer ( Token(..)
                        , Tok(..)
                        , Pos
                        , Line
                        , tokenize ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text

data Token = Token Pos Tok
  deriving (Show, Eq)

type Pos = (Int, Int) -- line, col

data Tok = TStr Text | TSpaces Int | TTab | TSym Char
  deriving (Show, Eq)

type Line = [Token]

tokenize :: Text -> [Line]
tokenize inp = zipWith tokenizeLine [1..] (Text.lines inp)

tokenizeLine :: Int -> Text -> [Token]
tokenizeLine linenum inp =
  go linenum 0 inp

  where go :: Int -> Int -> Text -> [Token]
        go linenum colnum t =
           if Text.null t
              then []
              else let (nexttok, rest, curcol) = getToken linenum colnum t
                   in nexttok `seq` (nexttok : go linenum curcol rest)

getToken :: Int -> Int -> Text -> (Token, Text, Int)
getToken curline curcol inp =
  case Text.uncons inp of
     Nothing        -> error "getToken called with empty input"
     Just (c, rest) ->
       let pos = (curline, curcol)
       in case c of
            ' ' -> let numspaces = maybe 1 (1 +) (Text.findIndex (/=' ') rest)
                   in (Token pos (TSpaces numspaces), rest, curcol + numspaces)
            '\t' -> let increment = 4 - curcol `mod` 4
                    in  (Token pos TTab, rest, curcol + increment)
            _ | isAlphaNum c ->
                    case Text.span isAlphaNum inp of
                         (cs, rest) ->
                            let numchars = Text.length cs
                            in  (Token pos (TStr cs), rest, curcol + numchars)
              | otherwise -> (Token pos (TSym c), rest, curcol + 1)

