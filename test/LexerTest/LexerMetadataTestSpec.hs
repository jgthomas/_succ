
module LexerTest.LexerMetadataTestSpec (lexerMetadataTest) where


import Data.Either
import Test.Hspec

import Lexer.Lexer
import LexerTest.TestUtility (metaData)
import Types.Tokens


lexerMetadataTest :: IO ()
lexerMetadataTest = hspec $ do
        describe "Lexing input captures correct metadata" $ do

                it "Should correctly extract metadata for syntactic tokens" $
                  (map metaData $ fromRight []
                   (tokenize "(\n)\n"))
                  `shouldBe`
                  [("(", 1), (")", 2)]

                it "Should record the correct line for each token" $
                  (map (line . tokenData) $
                  fromRight [] (tokenize "int main() { \n return 2;\n}"))
                  `shouldBe` [1,1,1,1,1,2,2,2,3]
