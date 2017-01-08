module CommonMark.Lexer ( tokenize ) where

import CommonMark.Types
import Data.Char (isAlphaNum, isAlpha, isDigit, isHexDigit, isAscii,
                  isSymbol, isPunctuation)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

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
           len = Text.length rest
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
            '*' ->  let n = maybe len id (Text.findIndex (/='*') rest)
                    in  (Token pos (TEmphChars Asterisk (n+1) (n+1)),
                          Text.drop n rest, curline, curcol + n + 1)
            '_' ->  let n = maybe len id (Text.findIndex (/='_') rest)
                    in  (Token pos (TEmphChars Underscore (n+1) (n+1)),
                          Text.drop n rest, curline, curcol + n + 1)
            '`' ->  let n = maybe len id (Text.findIndex (/='`') rest)
                    in  (Token pos (TBackticks (n+1)), Text.drop n rest,
                          curline, curcol + n + 1)
            '\\' -> case Text.uncons rest of
                         Just (c, rest') |
                           isAscii c && (isSymbol c || isPunctuation c) ->
                             (Token pos (TEscaped c), Text.drop 1 rest,
                               curline, curcol + 2)
                         _ -> (Token pos (TSym '\\'), rest, curline, curcol + 1)
            '&' ->  case scanEntity rest of
                         Just (ent, rest') ->
                           (Token pos (TEntity ent), rest',
                             curline, curcol + Text.length ent)
                         Nothing -> (Token pos (TSym '&'), rest,
                                        curline, curcol + 1)
            _ | isAlphaNum c ->
                    case Text.span isAlphaNum inp of
                         (cs, rest') ->
                            let numchars = Text.length cs
                            in  (Token pos (TStr cs), rest', curline,
                                  curcol + numchars)
              | otherwise -> (Token pos (TSym c), rest, curline, curcol + 1)

-- scan for an entity after the &; return full entity, including &,
-- and the remaining text.
scanEntity :: Text -> Maybe (Text, Text)
scanEntity t =
  let mbbody =
       case Text.uncons t of
         Just ('#', rest') ->
           case Text.uncons rest' of
                Just (c, rest'')
                  | c == 'x' || c == 'X' ->
                    case Text.span isHexDigit rest'' of
                         (es, rest''') | not (Text.null es) ->
                           Just (Text.singleton '#' <> Text.singleton c <> es,
                                 rest''')
                         _ -> Nothing
                  | isDigit c ->
                     case Text.span isDigit rest' of
                          (es, rest''') -> Just (Text.singleton '#' <> es,
                                                 rest''')
                Nothing -> Nothing
         _ ->
           case Text.span isAlpha t of
                (es, rest'')
                  | not (Text.null es)
                     -> Just (es, rest'')
                _ -> Nothing
  in  case mbbody of
           Nothing  -> Nothing
           Just (entbody, rest) ->
             case Text.uncons rest of
                  Just (';', remainder) ->
                    Just (Text.singleton '&' <> entbody <> Text.singleton ';',
                          remainder)
                  _ -> Nothing
