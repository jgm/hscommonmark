{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import CommonMark.Lexer

main :: IO ()
main = defaultMain $ testGroup "CommonMark tests" $
    [ testGroup "tokenizer tests" [
       testCase "lexer handles tab positions" $
          tokenize "hi\tthere" @=?
           [[Token (1,0) (TStr "hi")
            ,Token (1,2) TTab
            ,Token (1,4) (TStr "there")]]
      ]
    ]
