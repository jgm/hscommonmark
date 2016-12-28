{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module CommonMark.Types (
    Token(..)
  , tokenToText
  , Pos
  , Tok(..)
  , Line
  , BlockTree
  , Block(..)
  , BlockType(..) ) where

import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Generics (Data, Typeable)
import GHC.Generics (Generic)

data Token = Token Pos Tok
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

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
         | TLessThan
         | TGreaterThan
         | TBackslash
         | TNewline
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

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
       TLessThan -> Text.singleton '<'
       TGreaterThan -> Text.singleton '>'
       TBackslash -> Text.singleton '\\'
       TNewline -> Text.singleton '\n'

type Line = [Token]

type BlockTree = TreePos Full Block

data Block = Block { blockType   :: BlockType
                   , delimToks   :: [Token]
                   , contentToks :: [Token]
                   }
  deriving (Eq, Show)

data BlockType = Document
               | BlockQuote
               | List
               | Item
               | Paragraph
               | Heading
               | CodeBlock { codeIndented   :: Bool
                           , codeInfoString :: Text }
               | HtmlBlock
               | ThematicBreak
  deriving (Eq, Show)


