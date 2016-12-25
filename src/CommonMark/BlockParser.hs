module CommonMark.BlockParser ()
where
import CommonMark.Lexer (Line, Token(..), tokenize)

parseBlocks :: [Line] -> BlockTree
parseBlocks = undefined

data BlockTree = BlockTree

