
module ParserTest.ParserSpec (fullParserTest) where


import Test.Hspec

import ParserTest.TestUtility (extractFullProgramTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Operator
import Types.Tokens
import Types.Type


fullParserTest :: IO ()
fullParserTest = hspec $ do
        describe "Build abstract syntax trees for full programs" $ do

                it "Should build a tree for a simple program returning a constant" $
                  (extractFullProgramTree [Keyword Int,
                                           Ident "main",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           OpenBracket OpenBrace,
                                           Keyword Return,
                                           ConstInt 2,
                                           SemiColon,
                                           CloseBracket CloseBrace])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [ReturnNode (ConstantNode 2 mockNodeDat) mockNodeDat])
                               mockNodeDat]

                it "Should build a tree for a program with two functions" $
                  (extractFullProgramTree [Keyword Int,
                                           Ident "dog",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           OpenBracket OpenBrace,
                                           Keyword Return,
                                           ConstInt 2,
                                           SemiColon,
                                           CloseBracket CloseBrace,
                                           Keyword Int,
                                           Ident "main",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           OpenBracket OpenBrace,
                                           Keyword Return,
                                           Ident "dog",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           SemiColon,
                                           CloseBracket CloseBrace])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "dog"
                               []
                               (Just [ReturnNode
                                      (ConstantNode
                                       2
                                       mockNodeDat)
                                      mockNodeDat]
                               )
                               mockNodeDat,
                               FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [ReturnNode
                                      (FuncCallNode
                                       "dog"
                                       []
                                       mockNodeDat)
                                      mockNodeDat
                                     ]
                               )
                               mockNodeDat]

                it "Should build a tree for a program with a global variable and a function" $
                  (extractFullProgramTree [Keyword Int,
                                           Ident "a",
                                           OpTok EqualSign,
                                           ConstInt 2,
                                           SemiColon,
                                           Keyword Int,
                                           Ident "main",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           OpenBracket OpenBrace,
                                           Keyword Return,
                                           Ident "a",
                                           SemiColon,
                                           CloseBracket CloseBrace])
                  `shouldBe`
                  ProgramNode [DeclarationNode
                               (VarNode "a" mockNodeDat)
                               IntVar
                               (Just (AssignmentNode
                                      (VarNode "a" mockNodeDat)
                                      (ConstantNode 2 mockNodeDat)
                                      Assignment
                                      mockNodeDat)
                               )
                               mockNodeDat,
                               FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [(ReturnNode
                                      (VarNode
                                       "a"
                                       mockNodeDat)
                                      mockNodeDat)
                                     ]
                               )
                               mockNodeDat]
