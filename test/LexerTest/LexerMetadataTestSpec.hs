
module LexerTest.LexerMetadataTestSpec (lexerMetadataTest) where


import Data.Either
import Test.Hspec

import Lexer.Lexer
import Types.Tokens


lexerMetadataTest:: IO ()
lexerMetadataTest = hspec $ do
        describe "Capture correct metadata on lexing" $ do

                it "Should record the correct line for each token" $
                  (map (line . tokenData) $
                  fromRight [] (tokenize "int main() { \n return 2;\n}"))
                  `shouldBe` [1,1,1,1,1,2,2,2,3]
