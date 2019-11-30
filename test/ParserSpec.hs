
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
                it "Should parse valid variable declaration" $
                  fromRight (ProgramNode []) (parse [TokKeyword Int, TokIdent "a", TokSemiColon])
                  `shouldBe`
                  (ProgramNode [VarNode "a"])

                it "Should throw error on invalid variable identifier" $
                  fromLeft ImpossibleError (parse [TokKeyword Int, TokOpenBrace, TokSemiColon])
                  `shouldBe`
                  (SyntaxError (InvalidIdentifier TokOpenBrace))

                it "Should throw error on empty input" $
                  fromLeft ImpossibleError (parse [])
                  `shouldBe`
                  ParserError NoTokens
