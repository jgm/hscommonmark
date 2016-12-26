module CommonMark.BlockParser (
    BlockTree
  , Block(..)
  , BlockType(..)
  , parseBlocks
  , parseLine
  , analyzeLine
)
where
import CommonMark.Types (Line, Token(..), Tok(..))
import CommonMark.Lexer (tokenize)
import Control.Monad
import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace

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
  error $ show res
  where res = analyzeLine (reverse (ancestors treepos)) line

-- | Analyze line and return treepos of last matched
-- container node plus the remainder of the line,
-- after matched delimiters.
analyzeLine :: [BlockTree] -> Line -> Maybe (BlockTree, Line)
analyzeLine [] line = Nothing
analyzeLine (n:ns) line =
  case matchMarker n line of
       Nothing    -> Nothing
       Just line' -> Just $ go ns (n, line')
  where go []       (m, l) = (m, l)
        go (k:rest) (m, l) = case matchMarker k l of
                                  Nothing -> (m, l)
                                  Just l' -> go rest (k, l')

matchMarker :: BlockTree -> Line -> Maybe Line
matchMarker treepos line = do
  let tree' = tree treepos
      block = rootLabel tree'
  case blockType block of
       BDocument      -> return line
       BBlockQuote    ->
         case removeBlockQuoteStart line of
              Just (ds, rest) -> return rest
              Nothing -> mzero
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

removeBlockQuoteStart :: [Token] -> Maybe ([Token], [Token])
removeBlockQuoteStart ts = removeOneLeadingSpace <$>
  case ts of
       (Token p1 (TSym '>') : xs) ->
         return ([Token p1 (TSym '>')], xs)
       (Token p1 (TSpaces n) : Token p2 (TSym '>') : xs)
         | n < 4 -> return ([Token p1 (TSpaces n), Token p2 (TSym '>')], xs)
       _ -> mzero

removeOneLeadingSpace :: ([Token], [Token]) -> ([Token], [Token])
removeOneLeadingSpace (ts, Token (l,c) (TSpaces n) : xs)
  | n > 1 = (ts ++ [Token (l,c) (TSpaces 1)],
             Token (l,c+1) (TSpaces (n - 1)) : xs)
  | otherwise = (ts, xs)
removeOneLeadingSpace (ts, Token (l,c) TTab : xs) =
  let remaining = 4 - (c `mod` 4)
  in  if remaining > 1
         then (ts ++ [Token (l,c) (TSpaces 1)],
               Token (l,c+1) (TSpaces (remaining - 1)) : xs)
         else (ts, xs)
removeOneLeadingSpace (ts, xs) = (ts, xs)

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


