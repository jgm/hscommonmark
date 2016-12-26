{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import CommonMark.Lexer
import CommonMark.Types
import CommonMark.BlockParser

main :: IO ()
main = defaultMain $ testGroup "CommonMark tests" $
    [ testGroup "tokenizer tests" [
       testCase "lexer handles lines" $
          tokenize "hi\nthere\n\nbud\nok" @=?
          [[Token (1,0) (TStr "hi")]
          ,[Token (2,0) (TStr "there")]
          ,[]
          ,[Token (4,0) (TStr "bud")]
          ,[Token (5,0) (TStr "ok")]]
     , testCase "lexer handles tab positions" $
          tokenize "hi\ta \t\tb" @=?
           [[Token (1,0) (TStr "hi")
            ,Token (1,2) TTab
            ,Token (1,4) (TStr "a")
            ,Token (1,5) (TSpaces 1)
            ,Token (1,6) TTab
            ,Token (1,8) TTab
            ,Token (1,12) (TStr "b")]]
     , testCase "lexer handles symbols" $
          tokenize "h5i?@=" @=?
           [[Token (1,0) (TStr "h5i")
            ,Token (1,3) (TSym '?')
            ,Token (1,4) (TSym '@')
            ,Token (1,5) (TSym '=')]]
     , testCase "lexer handles empty input" $
          tokenize "" @=? []
      ]
    ]
