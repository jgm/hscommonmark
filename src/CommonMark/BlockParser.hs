{-# LANGUAGE RankNTypes #-}
module CommonMark.BlockParser (
    BlockTree
  , Block
  , Elt(..)
  , BlockType(..)
  , gobbleSpaces
  , parseBlocks
  , parseLine
  , analyzeLine
)
where
import CommonMark.Types
import CommonMark.Lexer (tokenize)
import Control.Monad
import Data.Tree.Zipper
import Data.Tree
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.Split (split, keepDelimsR, whenElt)
import Data.List (foldl')
import Debug.Trace

-- TODO
-- [ ] lists and items
-- [ ] thematic breaks
-- [ ] atx headers
-- [ ] setext headers
-- [ ] fenced code blocks
-- [ ] raw HTML blocks
-- [ ] reference link defs
-- [ ] resolving inline content

parseBlocks :: [Token] -> Tree Block
parseBlocks = toTree . root . foldl' parseLine (fromTree emptyDoc) . splitLines

splitLines :: [Token] -> [Line]
splitLines = split (keepDelimsR (whenElt isNewline))
              where isNewline (Token _ (TEndline _)) = True
                    isNewline _                      = False

parseLine :: BlockTree -> Line -> BlockTree
parseLine treepos line =
  case analyzeLine (reverse (ancestors treepos)) line of
       Nothing  -> error "analyzeLine returned Nothing" -- shouldn't happen
       Just (lastMatched, rest) ->
         let (tip, rest') = if isContainerBlock lastMatched
                               then openNewBlocks lastMatched rest
                               else (lastMatched, rest)
         in  addTokens tip rest'

isContainerBlock :: BlockTree -> Bool
isContainerBlock bt = case eltType (label bt) of
                      Document   -> True
                      BlockQuote -> True
                      List       -> True
                      Item       -> True
                      _          -> False

acceptsTokens :: BlockTree -> Bool
acceptsTokens bt = case eltType (label bt) of
                   Paragraph   -> True
                   BlankLines  -> True
                   Heading     -> True
                   CodeBlock{} -> True
                   HtmlBlock   -> True
                   _           -> False

-- We can assume that the treepos is a container block.
openNewBlocks :: BlockTree -> Line -> (BlockTree, Line)
openNewBlocks treepos line =
  case matchNewBlock line of
       Just (delim, rest, newblock) ->
         openNewBlocks (addChild newblock treepos) rest
       Nothing -> (treepos, line)

-- and return the child
addChild :: Tree Block -> BlockTree -> BlockTree
addChild newblock treepos =
  insert newblock (Data.Tree.Zipper.last (children treepos))

addTokens :: BlockTree -> Line -> BlockTree
addTokens treepos line =
  if acceptsTokens treepos
     then addContentToks line treepos
     else addContentToks line $
       if isBlank line
          then addChild emptyBlankLines treepos
          else case findOpenParagraph treepos of
                    Just para -> para
                    Nothing   -> addChild emptyParagraph treepos
  where addContentToks ts = modifyLabel (\label ->
               label{ contentToks = contentToks label ++ ts })

-- find a Paragraph that is the descendent of treepos by lastChild.
findOpenParagraph :: BlockTree -> Maybe BlockTree
findOpenParagraph treepos
  | eltType (label treepos) == Paragraph =
    Just treepos
  | otherwise =
    case lastChild treepos of
         Just child -> findOpenParagraph child
         Nothing    -> Nothing

matchNewBlock :: [Token] -> Maybe ([Token], [Token], Tree Block)
matchNewBlock ts =
  case gobbleSpaces 4 ts of
       Just rest -> return ([], rest, emptyCodeBlock)
       Nothing   ->
         case dropWhile isSpaceToken ts of
              (t@(Token pos (TSym '>')) : xs) -> do
                 let delim = maybe [t] (t:) (gobbleSpaces 1 xs)
                 let rest' = drop (length delim - 1) xs
                 return (delim, rest', emptyBlockQuote)
              _ -> mzero

isSpaceToken :: Token -> Bool
isSpaceToken (Token _ TSpace) = True
isSpaceToken (Token _ TTab)   = True
isSpaceToken _                = False

emptyDoc :: Tree Block
emptyDoc = Node (Elt Document [] []) []

emptyCodeBlock :: Tree Block
emptyCodeBlock = Node (Elt CodeBlock{ codeIndented = True
                                    , codeInfoString = Text.pack ""
                                    } [] []) []

emptyBlockQuote :: Tree Block
emptyBlockQuote = Node (Elt BlockQuote [] []) []

emptyParagraph :: Tree Block
emptyParagraph = Node (Elt Paragraph [] []) []

emptyBlankLines :: Tree Block
emptyBlankLines = Node (Elt BlankLines [] []) []

-- | Analyze line and return treepos of last matched
-- container node plus the remainder of the line,
-- after matched delimiters.
analyzeLine :: [BlockTree] -> Line -> Maybe (BlockTree, Line)
analyzeLine [] line = Nothing
analyzeLine (n:ns) line =
  case matchMarker n line of
       Nothing          -> Nothing
       Just (ds, line') -> Just $ go ns (addDelims ds n, line')
  where go []       (m, l) = (m, l)
        go (k:rest) (m, l) = case matchMarker k l of
                                  Nothing        -> (m, l)
                                  Just (ds', l') -> go rest
                                                    (addDelims ds' k, l')

addDelims :: [Token] -> BlockTree -> BlockTree
addDelims [] treepos = treepos
addDelims ds treepos = modifyLabel
  (\label -> label{ delimToks = delimToks label ++ ds }) treepos

matchMarker :: BlockTree -> [Token] -> Maybe ([Token], [Token])
matchMarker treepos line = do
  let block = label treepos
  case eltType block of
       Document      -> return ([], line)
       BlockQuote    -> removeBlockQuoteStart line
       List          -> return ([], line)
       Item          -> undefined  -- TODO
       Paragraph     -> do
         guard (not (isBlank line))
         return ([], line)
       BlankLines    -> do
         guard (isBlank line)
         return ([], line)
       Heading       -> mzero
       CodeBlock { codeIndented = indented }
         | indented  -> case gobbleSpaces 4 line of
                             Nothing  -> mzero
                             Just line' -> return ([], line')
         | otherwise -> return ([], line)
       HtmlBlock     -> undefined  -- TODO
       ThematicBreak -> mzero

gobbleSpaces :: Int -> [Token] -> Maybe [Token]
gobbleSpaces 0 ts = Just ts
gobbleSpaces n (Token _ TSpace : xs) =
  gobbleSpaces (n - 1) xs
gobbleSpaces n (Token (ln,col) TTab : xs) =
  case 4 - (col `mod` 4) of
       remaining | remaining < n  -> gobbleSpaces (n - remaining) xs
                 | remaining == n -> Just xs
                 | remaining > n  -> gobbleSpaces n (newspaces ++ xs)
                     where newspaces = map (\c -> Token (ln,c) TSpace)
                                        [col..(col + remaining - 1)]
gobbleSpaces n _ = Nothing

skipSpaces :: [Token] -> [Token]
skipSpaces (Token _ TSpace : xs) = skipSpaces xs
skipSpaces (Token _ TTab : xs) = skipSpaces xs
skipSpaces xs = xs

removeBlockQuoteStart :: [Token] -> Maybe ([Token], [Token])
removeBlockQuoteStart ts = removeOneLeadingSpace <$>
  case ts of
       (Token p1 (TSym '>') : xs) ->
          return ([Token p1 (TSym '>')], xs)
       (Token p1 TSpace : Token p2 (TSym '>') : xs) ->
          return ([Token p2 (TSym '>')], xs)
       _ -> mzero

removeOneLeadingSpace :: ([Token], [Token]) -> ([Token], [Token])
removeOneLeadingSpace (ts, Token (l,c) TSpace : xs) = (ts, xs)
removeOneLeadingSpace (ts, Token (l,c) TTab : xs) =
  let remaining = 4 - (c `mod` 4)
  in  if remaining > 1
         then (ts ++ [Token (l,c) TSpace],
               map (\n -> Token (l, c+n) TSpace) [1..(remaining - 1)] ++ xs)
         else (ts, xs)
removeOneLeadingSpace (ts, xs) = (ts, xs)

isBlank :: Line -> Bool
isBlank toks = all isBlankTok toks

isBlankTok :: Token -> Bool
isBlankTok (Token _ t) =
  case t of
       TSpace     -> True
       TTab       -> True
       TEndline _ -> True
       _          -> False

ancestors :: BlockTree -> [BlockTree]
ancestors treepos =
  case parent treepos of
        Nothing   -> [treepos]
        Just tp   -> tp : ancestors tp

