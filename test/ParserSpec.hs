
module ParserSpec (parserTest) where


import Data.Either
import Test.Hspec

import Parser
import Tokens
import AST
import Error
import VarTypes


parserTest :: IO ()
parserTest = hspec $ do
        describe "Parse tokens to AST" $ do
                it "Should parse valid variable declaration" $
                  fromRight (ProgramNode []) (parse [TokKeyword Int, TokIdent "a", TokSemiColon])
                  `shouldBe`
                  (ProgramNode [DeclarationNode "a" IntVar Nothing])

                it "Should throw error on invalid variable identifier" $
                  fromLeft ImpossibleError (parse [TokKeyword Int, TokOpenBrace, TokSemiColon])
                  `shouldBe`
                  (SyntaxError (InvalidIdentifier TokOpenBrace))

                it "Should throw error on empty input" $
                  fromLeft ImpossibleError (parse [])
                  `shouldBe`
                  ParserError (TokensError [])

                it "Should throw error on invalid type" $
                  fromLeft ImpossibleError (parse [TokKeyword Break, TokIdent "a", TokSemiColon])
                  `shouldBe`
                  (TypeError (InvalidType  (TokKeyword Break)))

                it "Should throw error if semicolon not final token" $
                  fromLeft ImpossibleError (parse [TokKeyword Int, TokIdent "a", TokWut])
                  `shouldBe`
                  SyntaxError (MissingToken TokSemiColon)

                it "Should correctly parse a simple function" $
                  fromRight (ProgramNode []) (parse [TokKeyword Int,
                                                     TokIdent "main",
                                                     TokOpenParen,
                                                     TokCloseParen,
                                                     TokOpenBrace,
                                                     TokKeyword Return,
                                                     TokConstInt 2,
                                                     TokSemiColon,
                                                     TokCloseBrace])
                  `shouldBe`
                  (ProgramNode [FunctionNode IntVar "main" [] (Just [ReturnNode (ConstantNode 2)])])

                it "Should parse valid variable assignment" $
                  fromRight (ProgramNode []) (parse [TokKeyword Int,
                                                     TokIdent "a",
                                                     TokOp Assign,
                                                     TokConstInt 10,
                                                     TokSemiColon])
                  `shouldBe`
                  (ProgramNode [DeclarationNode "a" IntVar (Just (AssignmentNode "a" (ConstantNode 10) Assign))])

                it "Should parse valid function declarations" $
                  fromRight (ProgramNode []) (parse [TokKeyword Int,
                                                     TokIdent "cat",
                                                     TokOpenParen,
                                                     TokKeyword Int,
                                                     TokIdent "a",
                                                     TokCloseParen,
                                                     TokSemiColon])
                  `shouldBe`
                  (ProgramNode [FunctionNode IntVar "cat" [ParamNode IntVar (VarNode "a")] Nothing])
