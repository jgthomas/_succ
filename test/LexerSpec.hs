
module LexerSpec (lexerTest) where


import Data.Either
import Test.Hspec

import Lexer
import Tokens
import Error


lexerTest :: IO ()
lexerTest = hspec $ do
        describe "lex tokens" $ do
                it "simple token of a single variable" $
                  fromRight [] (tokenize "int a;") `shouldBe` [TokKeyword Int,TokIdent "a",TokSemiColon]

                it "should be a two-character operator then a single one I" $
                  fromRight [] (tokenize "+=+") `shouldBe` [TokOp PlusAssign,TokOp Plus]

                it "should be a two character operator then a single one II" $
                  fromRight [] (tokenize "+==") `shouldBe` [TokOp PlusAssign,TokOp Assign]

                it "should be two of the SAME two-character operators" $
                  fromRight [] (tokenize "+=+=") `shouldBe` [TokOp PlusAssign,TokOp PlusAssign]

                it "should throw error for unrecognised character" $
                  fromLeft ImpossibleError (tokenize "$") `shouldBe` LexerError (BadInput "$")

                it "should throw error for empty input" $
                  fromLeft ImpossibleError (tokenize "") `shouldBe` LexerError EmptyInput
