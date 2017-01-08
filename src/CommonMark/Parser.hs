module CommonMark.Parser ( parseCommonMark )
where
import CommonMark.InlineParser
import CommonMark.BlockParser
import CommonMark.Types
import CommonMark.Lexer
import Data.Text (Text)
import Data.Tree (Tree(..))
import Data.Monoid

parseCommonMark :: Text -> Tree Block
parseCommonMark = resolveInlines mempty . parseBlocks . tokenize

resolveInlines :: RefMap -> Tree Block -> Tree Block
resolveInlines refmap (Node elt ns) = Node
  elt{ eltType =
        (case eltType elt of
              et@(Paragraph{}) -> et{
                  paragraphContents = parseInlines refmap (contentToks elt)
                }
              et@(Heading{}) -> et{
                  headingContents = parseInlines refmap (contentToks elt)
                }
              et           -> et) }
   (map (resolveInlines refmap) ns)
