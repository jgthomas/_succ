module LexerTest.LexerErrorTestSpec
  ( lexerErrorTest,
  )
where

import Data.Either
import Lexer.Lexer
import Test.Hspec
import Types.Error

lexerErrorTest :: IO ()
lexerErrorTest = hspec $ do
  describe "Lexing bad input throws correct errors" $ do
    it "Should throw error for unrecognised character" $
      fromLeft ImpossibleError (tokenize "$")
        `shouldBe` LexerError (UnexpectedInput "$")
    it "Should throw error for empty input" $
      fromLeft ImpossibleError (tokenize "")
        `shouldBe` LexerError EmptyInput
