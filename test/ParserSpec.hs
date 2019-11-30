
module ParserSpec (parserTest) where


import Data.Either
import Test.Hspec

import NewParser
import Tokens
import AST
import Error


parserTest :: IO ()
parserTest = hspec $ do
        describe "Parse tokens to AST" $ do
                it "Should parse tokens" $
                  fromRight (ProgramNode []) (parse [TokSemiColon])
                  `shouldBe`
                  (ProgramNode [VarNode "yes"])

                it "Should throw error on empty input" $
                  fromLeft ImpossibleError (parse [])
                  `shouldBe`
                  ParserError NoTokens
