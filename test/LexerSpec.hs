
module LexerSpec where


import Test.Hspec
import Lexer
import Tokens


main :: IO ()
main = hspec $ do
        describe "lexes tokens" $ do
                it "simple token of a single variable" $
                  tokenize "int a;" `shouldBe` [TokKeyword Int,TokIdent "a",TokSemiColon]

                it "should be a two character operator then a single one" $
                  tokenize "+=+" `shouldBe` [TokOp PlusAssign,TokOp Plus]

                it "should be a two character operator then a single one" $
                  tokenize "+==" `shouldBe` [TokOp PlusAssign,TokOp Assign]
