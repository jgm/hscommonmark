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
resolveInlines refmap (Node elt ns) =
  let contents = parseInlines refmap (removeFinalEndline (contentToks elt))
  in  Node elt{ eltType =
        (case eltType elt of
              et@(Paragraph{}) -> et{ paragraphContents = contents }
              et@(Heading{}) -> et{ headingContents = contents }
              et           -> et) }
        (map (resolveInlines refmap) ns)

removeFinalEndline :: [Token] -> [Token]
removeFinalEndline ts =
  case reverse ts of
       (Token _ (TEndline _) : rest) -> init ts
       _ -> ts
