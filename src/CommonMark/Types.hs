{-# LANGUAGE OverloadedStrings #-}
module CommonMark.Types (
    Token(..)
  , tokenToText
  , Pos
  , Tok(..)
  , Line ) where
import Data.Text (Text)
import qualified Data.Text as Text

data Token = Token Pos Tok
  deriving (Show, Eq)

type Pos = (Int, Int) -- line, col

data Tok = TStr Text
         | TSpace
         | TTab
         | TSym Char
         | TBackticks Int
         | TAsterisks Int
         | TUnderscores Int
         | TOpenBracket
         | TCloseBracket
         | TOpenParen
         | TCloseParen
         | TOpenAngle
         | TCloseAngle
         | TBackslash
  deriving (Show, Eq)

tokenToText :: Token -> Text
tokenToText (Token _ t) =
  case t of
       TStr s  -> s
       TSpace -> Text.singleton ' '
       TTab -> Text.singleton '\t'
       TSym c -> Text.singleton c
       TBackticks i -> Text.replicate i "`"
       TAsterisks i -> Text.replicate i "*"
       TUnderscores i -> Text.replicate i "_"
       TOpenBracket -> Text.singleton '['
       TCloseBracket -> Text.singleton ']'
       TOpenParen -> Text.singleton '('
       TCloseParen -> Text.singleton ')'
       TOpenAngle -> Text.singleton '<'
       TCloseAngle -> Text.singleton '>'
       TBackslash -> Text.singleton '\\'

type Line = [Token]


