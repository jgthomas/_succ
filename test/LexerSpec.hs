
module LexerSpec (lexerTest) where


import Data.Either
import Test.Hspec

import Lexer
import Tokens


lexerTest :: IO ()
lexerTest = hspec $ do
        describe "lexes tokens" $ do
                it "simple token of a single variable" $
                  fromRight [] (tokenize "int a;") `shouldBe` [TokKeyword Int,TokIdent "a",TokSemiColon]

                it "should be a two character operator then a single one I" $
                  fromRight [] (tokenize "+=+") `shouldBe` [TokOp PlusAssign,TokOp Plus]

                it "should be a two character operator then a single one II" $
                  fromRight [] (tokenize "+==") `shouldBe` [TokOp PlusAssign,TokOp Assign]

                it "should be two of the SAME two-character operators" $
                  fromRight [] (tokenize "+=+=") `shouldBe` [TokOp PlusAssign,TokOp PlusAssign]
