module CommonMark.Lexer ( tokenize, tokenizeLine ) where

import CommonMark.Types
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text

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
            ' ' ->  (Token pos TSpace, rest, curcol + 1)
            '\t' -> let increment = 4 - curcol `mod` 4
                    in  (Token pos TTab, rest, curcol + increment)
            '*' ->  let n = maybe 0 id (Text.findIndex (/='*') rest)
                    in  (Token pos (TAsterisks (n+1)), Text.drop n rest,
                           curcol + n + 1)
            '_' ->  let n = maybe 0 id (Text.findIndex (/='_') rest)
                    in  (Token pos (TUnderscores (n+1)), Text.drop n rest,
                          curcol + n + 1)
            '`' ->  let n = maybe 0 id (Text.findIndex (/='`') rest)
                    in  (Token pos (TBackticks (n+1)), Text.drop n rest,
                          curcol + n + 1)
            '\\' -> (Token pos TBackslash, rest, curcol + 1)
            '[' ->  (Token pos TOpenBracket, rest, curcol + 1)
            ']' ->  (Token pos TCloseBracket, rest, curcol + 1)
            '(' ->  (Token pos TOpenParen, rest, curcol + 1)
            ')' ->  (Token pos TCloseParen, rest, curcol + 1)
            '<' ->  (Token pos TOpenAngle, rest, curcol + 1)
            '>' ->  (Token pos TCloseAngle, rest, curcol + 1)
            _ | isAlphaNum c ->
                    case Text.span isAlphaNum inp of
                         (cs, rest') ->
                            let numchars = Text.length cs
                            in  (Token pos (TStr cs), rest', curcol + numchars)
              | otherwise -> (Token pos (TSym c), rest, curcol + 1)

