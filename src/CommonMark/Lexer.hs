module CommonMark.Lexer ( tokenize ) where

import CommonMark.Types
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text

tokenize :: Text -> [Token]
tokenize inp = go 1 0 inp
     where go :: Int -> Int -> Text -> [Token]
           go linenum colnum t =
             case getToken linenum colnum t of
                   Just (nexttok, rest, curline, curcol) ->
                      nexttok `seq` (nexttok : go curline curcol rest)
                   Nothing -> []

getToken :: Int -> Int -> Text -> Maybe (Token, Text, Int, Int)
getToken curline curcol inp =
  case Text.uncons inp of
     Nothing        -> Nothing
     Just (c, rest) -> Just $
       let pos = (curline, curcol)
       in case c of
            '\n' -> (Token pos (TEndline LF), rest, curline + 1, 0)
            '\r' -> case Text.uncons rest of
                         Just ('\n', rest') ->
                             (Token pos (TEndline CRLF), rest', curline + 1, 0)
                         _   ->
                             (Token pos (TEndline CR), rest, curline + 1, 0)
            ' ' ->  (Token pos TSpace, rest, curline, curcol + 1)
            '\t' -> let increment = 4 - curcol `mod` 4
                    in  (Token pos TTab, rest, curline, curcol + increment)
            '*' ->  let n = maybe 0 id (Text.findIndex (/='*') rest)
                    in  (Token pos (TAsterisks (n+1)), Text.drop n rest,
                           curline, curcol + n + 1)
            '_' ->  let n = maybe 0 id (Text.findIndex (/='_') rest)
                    in  (Token pos (TUnderscores (n+1)), Text.drop n rest,
                          curline, curcol + n + 1)
            '`' ->  let n = maybe 0 id (Text.findIndex (/='`') rest)
                    in  (Token pos (TBackticks (n+1)), Text.drop n rest,
                          curline, curcol + n + 1)
            _ | isAlphaNum c ->
                    case Text.span isAlphaNum inp of
                         (cs, rest') ->
                            let numchars = Text.length cs
                            in  (Token pos (TStr cs), rest', curline,
                                  curcol + numchars)
              | otherwise -> (Token pos (TSym c), rest, curline, curcol + 1)

