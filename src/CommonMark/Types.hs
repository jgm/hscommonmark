{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module CommonMark.Types (
    Token(..)
  , tokenToText
  , Pos
  , Tok(..)
  , EndlineType(..)
  , Line
  , BlockTree
  , Elt(..)
  , Block
  , Inline
  , BlockType(..)
  , InlineType(..)
  , Label
  , RefMap
  ) where

import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Generics (Data, Typeable)
import GHC.Generics (Generic)
import Data.CaseInsensitive (CI, mk)
import qualified Data.Map as Map

data Token = Token Pos Tok
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

type Pos = (Int, Int) -- line, col

data Tok = TStr Text
         | TSpace
         | TTab
         | TSym Char
         | TEntity Text
         | TEscaped Char
         | TBackticks Int
         | TAsterisks Int Int  -- first is orig length, second adjusted
         | TUnderscores Int Int -- first is orig length, second adjusted
         | TEndline EndlineType
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data EndlineType = CR | LF | CRLF
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | This function should obey the law:
-- tokenToText (tokenize x) == x
tokenToText :: Token -> Text
tokenToText (Token _ t) =
  case t of
       TStr s  -> s
       TSpace -> Text.singleton ' '
       TTab -> Text.singleton '\t'
       TSym c -> Text.singleton c
       TEntity s -> s
       TEscaped c -> Text.pack ['\\',c]
       TBackticks i -> Text.replicate i "`"
       TAsterisks _ i -> Text.replicate i "*"
       TUnderscores _ i -> Text.replicate i "_"
       TEndline LF -> Text.singleton '\n'
       TEndline CR -> Text.singleton '\r'
       TEndline CRLF -> Text.pack "\r\n"

type Line = [Token]

data Elt a = Elt { eltType     :: a
                 , delimToks   :: [Token]
                 , contentToks :: [Token]
                 }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

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
               | BlankLines
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data InlineType = Inlines
                | Txt
                | Space
                | Softbreak
                | Linebreak
                | Code
                | HtmlInline
                | Emph
                | Strong
                | Link{ linkDestination :: Text
                      , linkTitle :: Text }
                | Image{ imageSource :: Text
                       , imageTitle :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

type Block = Elt BlockType
type Inline = Elt InlineType

type BlockTree = TreePos Full Block
type InlineTree = TreePos Full Inline

type Label = CI Text

type RefMap = Map.Map Label (Text, Text)

