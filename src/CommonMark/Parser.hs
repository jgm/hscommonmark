module CommonMark.Parser ( parseCommonMark )
where
import CommonMark.InlineParser
import CommonMark.BlockParser
import CommonMark.Lexer
import Data.Text (Text)
import Data.Tree (Tree)

parseCommonMark :: Text -> Tree Block
parseCommonMark = undefined

