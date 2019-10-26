
module LexerSpec (lexerTest) where


import Test.Hspec
import Lexer
import Tokens


lexerTest :: IO ()
lexerTest = hspec $ do
        describe "lexes tokens" $ do
                it "simple token of a single variable" $
                  tokenize "int a;" `shouldBe` [TokKeyword Int,TokIdent "a",TokSemiColon]

                it "should be a two character operator then a single one I" $
                  tokenize "+=+" `shouldBe` [TokOp PlusAssign,TokOp Plus]

                it "should be a two character operator then a single one II" $
                  tokenize "+==" `shouldBe` [TokOp PlusAssign,TokOp Assign]

                it "should be two of the SAME two-character operators" $
                  tokenize "+=+=" `shouldBe` [TokOp PlusAssign,TokOp PlusAssign]
