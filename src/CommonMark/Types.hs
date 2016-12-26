module CommonMark.Types (
    Token(..)
  , Pos
  , Tok(..)
  , Line ) where
import Data.Text (Text)

data Token = Token Pos Tok
  deriving (Show, Eq)

type Pos = (Int, Int) -- line, col

data Tok = TStr Text | TSpaces Int | TTab | TSym Char
  deriving (Show, Eq)

type Line = [Token]


