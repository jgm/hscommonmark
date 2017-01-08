{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Text.Arbitrary ()
import CommonMark.Lexer
import CommonMark.Types
import CommonMark.BlockParser
import CommonMark.InlineParser
import Data.Tree
import Data.Tree.Zipper
import qualified Data.Text as Text

main :: IO ()
main = defaultMain $ testGroup "CommonMark tests" $
    [ testGroup "tokenizer tests" [
       testCase "lexer handles lines" $
          tokenize "hi\nthere\n\nbud\nok" @?=
          [Token (1,0) (TStr "hi"), Token (1,2) (TEndline LF)
          ,Token (2,0) (TStr "there"), Token (2,5) (TEndline LF)
          ,Token (3,0) (TEndline LF)
          ,Token (4,0) (TStr "bud"), Token (4,3) (TEndline LF)
          ,Token (5,0) (TStr "ok")]
      , testCase "lexer handles tab positions" $
          tokenize "hi\ta \t\tb" @?=
           [Token (1,0) (TStr "hi")
           ,Token (1,2) TTab
           ,Token (1,4) (TStr "a")
           ,Token (1,5) TSpace
           ,Token (1,6) TTab
           ,Token (1,8) TTab
           ,Token (1,12) (TStr "b")]
      , testCase "lexer handles symbols" $
          tokenize "h5i?@=" @?=
           [Token (1,0) (TStr "h5i")
           ,Token (1,3) (TSym '?')
           ,Token (1,4) (TSym '@')
           ,Token (1,5) (TSym '=')]
      , testCase "lexer handles asterisk runs" $
          tokenize "**a***" @?=
           [Token (1,0) (TAsterisks 2 2)
           ,Token (1,2) (TStr "a")
           ,Token (1,3) (TAsterisks 3 3)]
      , testCase "lexer handles empty input" $
          tokenize "" @?= []
      , testProperty "tokenToText round-trip" $
          \x -> (mconcat . map tokenToText . tokenize) x == x
      ]
    , testGroup "gobbleSpaces tests" [
        testCase "gobbleSpaces returns Nothing when not enough spaces" $
          gobbleSpaces 2 [Token (1,0) TSpace, Token (1,1) (TSym '$')] @?=
          Nothing
      , testCase "gobbleSpaces returns Just when enough spaces" $
          gobbleSpaces 2 [Token (1,0) TSpace, Token (1,1) TSpace,
                            Token (1,2) (TSym '$')] @?=
          Just [Token (1,2) (TSym '$')]
      , testCase "gobbleSpaces returns Just when more spaces" $
          gobbleSpaces 1 [Token (1,0) TSpace, Token (1,1) TSpace,
                            Token (1,2) (TSym '$')] @?=
          Just [Token (1,1) TSpace, Token (1,2) (TSym '$')]
      , testCase "gobbleSpaces handles tab" $
          gobbleSpaces 2 [Token (1,0) TSpace, Token (1,1) TTab,
             Token (1,4) (TSym '$')] @?=
          Just [Token (1,2) TSpace, Token (1,3) TSpace, Token (1,4) (TSym '$')]
      ]
    , testGroup "analyzeLine tests" [
        testCase "block quote starts 1" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TStr "hi"),Token (1,2) (TSym '>')]
            @?=
            Just (fromTree t4, [Token (1,0) (TStr "hi"),
                                Token (1,2) (TSym '>')])
      , testCase "block quote starts 2" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TSym '>'),Token (1,2) (TStr "hi"),
             Token (1,4) (TEndline LF)]
            @?=
            Just (fromTree t3{ rootLabel = (rootLabel t3){ delimToks =
               [Token (1,0) (TSym '>')] }}, [Token (1,2) (TStr "hi"),
                Token (1,4) (TEndline LF)])
      , testCase "block quote starts 3" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TSym '>'), Token (1,1) TSpace, Token (1,2) TSpace,
              Token (1,3) (TSym '>'), Token (1,4) (TStr "hi"),
              Token (1,6) (TEndline LF)]
            @?=
            Just (fromTree t1, [Token (1,4) (TStr "hi"), Token (1,6) (TEndline LF)])
      ]
    , testGroup "parseBlocks tests" [
        testCase "simple two-line paragraph" $
          parseBlocks (tokenize "a\nb") @?=
          Node {rootLabel = Elt {eltType = Document, delimToks = [], contentToks = []}, subForest = [Node {rootLabel = Elt {eltType = Paragraph, delimToks = [], contentToks = [Token (1,0) (TStr "a"),Token (1,1) (TEndline LF),Token (2,0) (TStr "b")]}, subForest = []}]}
      , testCase "paragraphs separated by blank line" $
          parseBlocks (tokenize "a\n\nb") @?=
          Node {rootLabel = Elt {eltType = Document, delimToks = [], contentToks = []}, subForest = [Node {rootLabel = Elt {eltType = Paragraph, delimToks = [], contentToks = [Token (1,0) (TStr "a"),Token (1,1) (TEndline LF)]}, subForest = []}, Node {rootLabel = Elt {eltType = BlankLines, delimToks = [], contentToks = [Token (2,0) (TEndline LF)]}, subForest = []}, Node {rootLabel = Elt {eltType = Paragraph, delimToks = [], contentToks = [Token (3,0) (TStr "b")]}, subForest = []}]}
      ]
    , testGroup "parseInlines tests" [
        testCase "code span with backslash + symbol" $
          forest (children (parseInlines mempty (tokenize "``h\\*``")))
          @?=
          [Node {rootLabel = Elt {eltType = Code, delimToks = [Token (1,0) (TBackticks 2),Token (1,5) (TBackticks 2)], contentToks = [Token (1,2) (TStr "h"),Token (1,3) (TStr "\\*")]}, subForest = []}]
      , testCase "code span with final backslash" $
          forest (children (parseInlines mempty (tokenize "``hi\\``")))
          @?=
          [Node {rootLabel = Elt {eltType = Code, delimToks = [Token (1,0) (TBackticks 2),Token (1,5) (TBackticks 2)], contentToks = [Token (1,2) (TStr "hi"),Token (1,4) (TSym '\\')]}, subForest = []}]
      , testCase "3 blanks + newline = linebreak" $
          forest (children (parseInlines mempty (tokenize "hi   \nthere")))
          @?=
          [Node {rootLabel = Elt {eltType = Txt, delimToks = [], contentToks = [Token (1,0) (TStr "hi")]}, subForest = []},Node {rootLabel = Elt {eltType = Linebreak, delimToks = [], contentToks = [Token (1,2) TSpace,Token (1,3) TSpace,Token (1,4) TSpace,Token (1,5) (TEndline LF)]}, subForest = []},Node {rootLabel = Elt {eltType = Txt, delimToks = [], contentToks = [Token (2,0) (TStr "there")]}, subForest = []}]
      , testCase "backslash + newline = linebreak" $
          forest (children (parseInlines mempty (tokenize "hi\\\nthere")))
          @?=
          [Node {rootLabel = Elt {eltType = Txt, delimToks = [], contentToks = [Token (1,0) (TStr "hi")]}, subForest = []},Node {rootLabel = Elt {eltType = Linebreak, delimToks = [Token (1,2) (TSym '\\')], contentToks = [Token (1,3) (TEndline LF)]}, subForest = []},Node {rootLabel = Elt {eltType = Txt, delimToks = [], contentToks = [Token (2,0) (TStr "there")]}, subForest = []}]
      ]
    ]

t1 :: Tree Block
t1 = Node (Elt Paragraph [] []) []

t2 :: Tree Block
t2 = Node (Elt BlockQuote [] []) [t1]

t3 :: Tree Block
t3 = Node (Elt BlockQuote [] []) [t2]

t4 :: Tree Block
t4 = Node (Elt Document [] []) [t3]

