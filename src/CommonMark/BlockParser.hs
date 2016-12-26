module CommonMark.BlockParser ()
where
import CommonMark.Types (Line, Token(..), Tok(..))
import CommonMark.Lexer (tokenize)
import Control.Monad
import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text

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
               | BCodeBlock { codeIndented   :: Bool
                            , codeInfoString :: Text }
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
matchMarker treepos line = do
  let tree' = tree treepos
      block = rootLabel tree'
  case blockType block of
       BDocument      -> return line
       BBlockQuote    ->
         case line of
              (Token _ (TSpaces n) : Token _ (TSym '>') : xs)
                | n < 4 -> return (removeOneLeadingSpace xs)
              (Token _ (TSym '>') : xs) ->
                return (removeOneLeadingSpace xs)
              (Token _ TTab : Token _ (TSym '>') : xs) ->
                return (removeOneLeadingSpace xs)
              _ -> mzero
       BList          -> return line
       BItem          -> undefined  -- TODO
       BParagraph     -> do
         guard (not (isBlank line))
         return line
       BHeading       -> mzero
       BCodeBlock { codeIndented = indented }
         | indented   ->
             case line of
                 (Token _ TTab : xs) -> return xs
                 (Token (l,c) (TSpaces n) : xs)
                   | n > 4 -> return (Token (l,c+4) (TSpaces (n - 4)) : xs)
                   | n == 4 -> return xs
                   | otherwise -> mzero
                 _  -> mzero
         | otherwise -> return line
       BHtmlBlock     -> undefined  -- TODO
       BThematicBreak -> mzero

removeOneLeadingSpace :: Line -> Line
removeOneLeadingSpace (Token (l,c) (TSpaces n) : xs)
  | n > 1 = Token (l,c+1) (TSpaces (n - 1)) : xs
  | otherwise = xs
removeOneLeadingSpace (Token (l,c) TTab : xs) =
  let remaining = 4 - (c `mod` 4)
  in  if remaining > 1
         then Token (l,c+1) (TSpaces (remaining - 1)) : xs
         else xs
removeOneLeadingSpace line = line

isBlank :: Line -> Bool
isBlank toks = all isBlankTok toks

isBlankTok :: Token -> Bool
isBlankTok (Token _ t) =
  case t of
       TSpaces _ -> True
       TTab      -> True
       _         -> False

ancestors :: BlockTree -> [BlockTree]
ancestors treepos =
  case parent treepos of
        Nothing   -> [treepos]
        Just tp   -> tp : ancestors tp

-- TODO remove:
test :: BlockTree
test = fromTree $
  Node (Block BDocument [] []) [
          Node (Block BBlockQuote [] []) [
            Node (Block BParagraph [] [])
            []
         ]
        ]


