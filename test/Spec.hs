{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Text.Arbitrary ()
import CommonMark.Lexer
import CommonMark.Types
import CommonMark.BlockParser
import Data.Tree
import Data.Tree.Zipper
import qualified Data.Text as Text

main :: IO ()
main = defaultMain $ testGroup "CommonMark tests" $
    [ testGroup "tokenizer tests" [
       testCase "lexer handles lines" $
          tokenize "hi\nthere\n\nbud\nok" @?=
          [[Token (1,0) (TStr "hi")]
          ,[Token (2,0) (TStr "there")]
          ,[]
          ,[Token (4,0) (TStr "bud")]
          ,[Token (5,0) (TStr "ok")]]
      , testCase "lexer handles tab positions" $
          tokenize "hi\ta \t\tb" @?=
           [[Token (1,0) (TStr "hi")
            ,Token (1,2) TTab
            ,Token (1,4) (TStr "a")
            ,Token (1,5) TSpace
            ,Token (1,6) TTab
            ,Token (1,8) TTab
            ,Token (1,12) (TStr "b")]]
      , testCase "lexer handles symbols" $
          tokenize "h5i?@=" @?=
           [[Token (1,0) (TStr "h5i")
            ,Token (1,3) (TSym '?')
            ,Token (1,4) (TSym '@')
            ,Token (1,5) (TSym '=')]]
      , testCase "lexer handles empty input" $
          tokenize "" @?= []
      , testCase "lexer handles empty input" $
          tokenize "" @?= []
      , testProperty "tokenToText round-trip" $
          \x -> (mconcat . map tokenToText . tokenizeLine 1) x == x
      ]
    , testGroup "gobbleSpaces tests" [
        testCase "gobbleSpaces returns Nothing when not enough spaces" $
          gobbleSpaces 2 [Token (1,0) TSpace, Token (1,0) (TSym '$')] @?=
          Nothing
      ]
    , testGroup "analyzeLine tests" [
        testCase "block quote starts 1" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TStr "hi"),Token (1,2) (TSym '>')]
            @?=
            Just (fromTree t4, [Token (1,0) (TStr "hi"),Token (1,2) (TSym '>')])
      , testCase "block quote starts 2" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TSym '>'),Token (1,2) (TStr "hi")]
            @?=
            Just (fromTree t3, [Token (1,2) (TStr "hi")])
      , testCase "block quote starts 3" $
          analyzeLine [fromTree t4, fromTree t3, fromTree t2, fromTree t1]
            [Token (1,0) (TSym '>'), Token (1,1) TSpace, Token (1,2) TSpace,
              Token (1,3) (TSym '>'), Token (1,4) (TStr "hi")]
            @?=
            Just (fromTree t1, [Token (1,4) (TStr "hi")])
      ]
    ]

t1 :: Tree Block
t1 = Node (Block BParagraph [] []) []

t2 :: Tree Block
t2 = Node (Block BBlockQuote [] []) [t1]

t3 :: Tree Block
t3 = Node (Block BBlockQuote [] []) [t2]

t4 :: Tree Block
t4 = Node (Block BDocument [] []) [t3]

