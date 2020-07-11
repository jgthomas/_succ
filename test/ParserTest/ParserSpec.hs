
module ParserTest.ParserSpec (fullParserTest) where


import Test.Hspec

import ParserTest.TestUtility (extractFullProgramError, extractFullProgramTree)
import TestUtility            (makeLexDat, mockNodeDat)
import Types.AST
import Types.Error
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
                               (Just $ CompoundStmtNode
                                [ReturnNode
                                 (ConstantNode 2 mockNodeDat)
                                 mockNodeDat]
                                mockNodeDat
                               )
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
                               (Just $ CompoundStmtNode
                                [ReturnNode
                                 (ConstantNode
                                  2
                                  mockNodeDat)
                                 mockNodeDat]
                                mockNodeDat
                               )
                               mockNodeDat,
                               FunctionNode
                               IntVar
                               "main"
                               []
                               (Just $ CompoundStmtNode
                                [ReturnNode
                                  (FuncCallNode
                                   "dog"
                                   []
                                   mockNodeDat)
                                  mockNodeDat]
                                mockNodeDat
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
                               (Just
                                (AssignmentNode
                                 (VarNode "a" mockNodeDat)
                                 (ConstantNode 2 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                               )
                               mockNodeDat,
                               FunctionNode
                               IntVar
                               "main"
                               []
                               (Just $ CompoundStmtNode
                                [(ReturnNode
                                  (VarNode "a" mockNodeDat)
                                  mockNodeDat)]
                                mockNodeDat
                               )
                               mockNodeDat]

        describe "Throw errors on bad input" $ do

                it "Should throw an error on empty input" $
                  (extractFullProgramError [])
                  `shouldBe`
                  ParserError (LexDataError [])

                it "Should throw an error when a top level item doesn't start with a keyword" $
                  (extractFullProgramError [SemiColon, OpTok PlusSign])
                  `shouldBe`
                  ParserError (LexDataError $ map makeLexDat [SemiColon, OpTok PlusSign])
