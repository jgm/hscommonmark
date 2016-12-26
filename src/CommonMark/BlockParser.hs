module CommonMark.BlockParser ()
where
import CommonMark.Types (Line, Token(..))
import CommonMark.Lexer (tokenize)
import Data.Tree.Zipper
import Data.Tree

type BlockTree = TreePos Full Block

data Block = Block { blockType   :: BlockType
                   , delimToks   :: [Token]
                   , contentToks :: [Token]
                   }
  deriving (Eq, Show)

data BlockType = BDocument
               | BBlockQuote
               | BList
               | BItem
               | BParagraph
               | BHeading
               | BCodeBlock
               | BHtmlBlock
               | BThematicBreak
  deriving (Eq, Show)

emptyDoc :: Tree Block
emptyDoc = Node (Block BDocument [] []) []

parseBlocks :: [Line] -> Tree Block
parseBlocks = toTree . foldr parseLine (fromTree emptyDoc)

parseLine :: Line -> BlockTree -> BlockTree
parseLine line treepos =
  error $ show (rest, lastMatched)
  where (rest, lastMatched) = analyzeLine (reverse (ancestors treepos)) line

-- | Analyze line and return treepos of last matched
-- container node plus the remainder of the line,
-- after matched delimiters.
analyzeLine :: [BlockTree] -> Line -> (BlockTree, Line)
analyzeLine (tp:tps) line =
  case tps of
       []    -> (tp, line)
       (n:ns) -> case matchMarker n line of
                      Nothing   -> (tp, line)
                      Just rest -> analyzeLine ns rest
analyzeLine [] line = error "analyzeLine [] - should not happen"

matchMarker :: BlockTree -> Line -> Maybe Line
matchMarker treepos line =
  case blockType block of
       BDocument      -> Just line
       BBlockQuote    -> Nothing
       BList          -> Nothing
       BItem          -> Nothing
       BParagraph     -> Nothing
       BHeading       -> Nothing
       BCodeBlock     -> Nothing
       BHtmlBlock     -> Nothing
       BThematicBreak -> Nothing
  where tree' = tree treepos
        block = rootLabel tree'

ancestors :: BlockTree -> [BlockTree]
ancestors treepos =
  case parent treepos of
        Nothing   -> [treepos]
        Just tp   -> tp : ancestors tp

test :: BlockTree
test = fromTree $
  Node (Block BDocument [] []) [
          Node (Block BBlockQuote [] []) [
            Node (Block BParagraph [] [])
            []
         ]
        ]


