
module ParserSpec (parserTest) where


import Data.Either
import Test.Hspec

import AST
import Error
import Operator
import Parser
import Tokens
import Type


parserTest :: IO ()
parserTest = hspec $ do
        describe "Parse tokens to AST" $ do
                it "Should parse valid variable declaration" $
                  fromRight (ProgramNode []) (parse [Keyword Int, Ident "a", SemiColon])
                  `shouldBe`
                  (ProgramNode [DeclarationNode "a" IntVar Nothing])

                it "Should parse valid pointer declaration" $
                  fromRight (ProgramNode []) (parse [Keyword Int,
                                                   OpTok Asterisk,
                                                   Ident "a",
                                                   SemiColon])
                  `shouldBe`
                  (ProgramNode [PointerNode "a" IntPointer Nothing])

                it "Should parse valid variable assignment" $
                  fromRight (ProgramNode []) (parse [Keyword Int,
                                                     Ident "a",
                                                     OpTok EqualSign,
                                                     ConstInt 10,
                                                     SemiColon])
                  `shouldBe`
                  (ProgramNode [DeclarationNode "a" IntVar (Just (AssignmentNode "a" (ConstantNode 10) Assignment))])

                it "Should parse valid function declaration" $
                  fromRight (ProgramNode []) (parse [Keyword Int,
                                                     Ident "cat",
                                                     OpenParen,
                                                     Keyword Int,
                                                     Ident "a",
                                                     CloseParen,
                                                     SemiColon])
                  `shouldBe`
                  (ProgramNode [FunctionNode IntVar "cat" [ParamNode IntVar (VarNode "a")] Nothing])

                it "Should parse a valid function definition" $
                  fromRight (ProgramNode []) (parse [Keyword Int,
                                                     Ident "main",
                                                     OpenParen,
                                                     CloseParen,
                                                     OpenBrace,
                                                     Keyword Return,
                                                     ConstInt 2,
                                                     SemiColon,
                                                     CloseBrace])
                  `shouldBe`
                  (ProgramNode [FunctionNode IntVar "main" [] (Just [ReturnNode (ConstantNode 2)])])

        describe "Throw correct errors" $ do
                it "Should throw error on invalid variable identifier" $
                  fromLeft ImpossibleError (parse [Keyword Int, OpenBrace, SemiColon])
                  `shouldBe`
                  (SyntaxError (InvalidIdentifier OpenBrace))

                it "Should throw error on empty input" $
                  fromLeft ImpossibleError (parse [])
                  `shouldBe`
                  ParserError (TokensError [])

                it "Should throw error on invalid type" $
                  fromLeft ImpossibleError (parse [Keyword Break, Ident "a", SemiColon])
                  `shouldBe`
                  (TypeError (InvalidType  (Keyword Break)))

                it "Should throw error if semicolon not final token" $
                  fromLeft ImpossibleError (parse [Keyword Int, Ident "a", Comma])
                  `shouldBe`
                  SyntaxError (MissingToken SemiColon)
