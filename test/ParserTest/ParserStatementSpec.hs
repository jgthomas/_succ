
module ParserTest.ParserStatementSpec (parserStatementTest) where


import Test.Hspec

import ParserTest.TestUtility (extractStatementTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Operator
import Types.Tokens
import Types.Type


parserStatementTest :: IO ()
parserStatementTest = hspec $ do
        describe "Build abstract syntax trees for statements" $ do

                it "Should build a tree for a continue statement" $
                  (extractStatementTree [Keyword Continue, SemiColon])
                  `shouldBe`
                  ProgramNode [ContinueNode mockNodeDat]

                it "Should build a tree for a break statement" $
                  (extractStatementTree [Keyword Break, SemiColon])
                  `shouldBe`
                  ProgramNode [BreakNode mockNodeDat]

                it "Should build a tree for a return statement" $
                  (extractStatementTree [Keyword Return, ConstInt 2, SemiColon])
                  `shouldBe`
                  ProgramNode [ReturnNode
                               (ConstantNode 2 mockNodeDat)
                               mockNodeDat
                              ]

                it "Should build a tree for a null statement" $
                  (extractStatementTree [SemiColon])
                  `shouldBe`
                  ProgramNode [NullExprNode mockNodeDat]

                it "Should build a tree for a simple compound statement" $
                  (extractStatementTree [OpenBracket OpenBrace,
                                         ConstInt 2,
                                         OpTok PlusSign,
                                         ConstInt 2,
                                         SemiColon,
                                         Keyword Break,
                                         SemiColon,
                                         CloseBracket CloseBrace
                                        ])
                  `shouldBe`
                  ProgramNode [CompoundStmtNode
                               [
                                ExprStmtNode
                                (BinaryNode
                                 (ConstantNode 2 mockNodeDat)
                                 (ConstantNode 2 mockNodeDat)
                                 Plus
                                 mockNodeDat
                                )
                                mockNodeDat,
                                BreakNode mockNodeDat
                               ]
                               mockNodeDat
                              ]

                it "Should build a tree for compound statement with a declaration" $
                  (extractStatementTree [OpenBracket OpenBrace,
                                         ConstInt 2,
                                         OpTok PlusSign,
                                         ConstInt 2,
                                         SemiColon,
                                         Keyword Int,
                                         Ident "a",
                                         OpTok EqualSign,
                                         ConstInt 3,
                                         SemiColon,
                                         CloseBracket CloseBrace
                                        ])
                  `shouldBe`
                  ProgramNode [CompoundStmtNode
                               [ExprStmtNode
                                (BinaryNode
                                 (ConstantNode 2 mockNodeDat)
                                 (ConstantNode 2 mockNodeDat)
                                 Plus
                                 mockNodeDat
                                )
                                mockNodeDat,
                                DeclarationNode
                                (VarNode "a" mockNodeDat)
                                IntVar
                                (Just $ AssignmentNode
                                 (VarNode "a" mockNodeDat)
                                 (ConstantNode 3 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                                mockNodeDat
                               ]
                               mockNodeDat
                              ]


                it "Should build a tree for a simple if statement" $
                  (extractStatementTree [Keyword If,
                                         OpenBracket OpenParen,
                                         Ident "a",
                                         CloseBracket CloseParen,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 1,
                                         SemiColon
                                        ])
                  `shouldBe`
                  ProgramNode [IfNode
                               (VarNode "a" mockNodeDat)
                               (ExprStmtNode
                                (AssignmentNode
                                 (VarNode "b" mockNodeDat)
                                 (ConstantNode 1 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                                mockNodeDat
                               )
                               Nothing
                               mockNodeDat
                              ]

                it "Should build a tree for an if statement with a block" $
                  (extractStatementTree [Keyword If,
                                         OpenBracket OpenParen,
                                         Ident "a",
                                         CloseBracket CloseParen,
                                         OpenBracket OpenBrace,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 1,
                                         SemiColon,
                                         CloseBracket CloseBrace
                                        ])
                  `shouldBe`
                  ProgramNode [IfNode
                               (VarNode "a" mockNodeDat)
                               (CompoundStmtNode
                                [ExprStmtNode
                                 (AssignmentNode
                                  (VarNode "b" mockNodeDat)
                                  (ConstantNode 1 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                                mockNodeDat
                               )
                               Nothing
                               mockNodeDat
                              ]

                it "Should build a tree for a simple if statement with an else clause" $
                  (extractStatementTree [Keyword If,
                                         OpenBracket OpenParen,
                                         Ident "a",
                                         CloseBracket CloseParen,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 1,
                                         SemiColon,
                                         Keyword Else,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 2,
                                         SemiColon
                                        ])
                  `shouldBe`
                  ProgramNode [IfNode
                               (VarNode "a" mockNodeDat)
                               (ExprStmtNode
                                (AssignmentNode
                                 (VarNode "b" mockNodeDat)
                                 (ConstantNode 1 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                                mockNodeDat
                               )
                               (Just $ ExprStmtNode
                                (AssignmentNode
                                 (VarNode "b" mockNodeDat)
                                 (ConstantNode 2 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                                mockNodeDat
                               )
                               mockNodeDat
                              ]

                it "Should build a tree for an if statement with an else clause and a block" $
                  (extractStatementTree [Keyword If,
                                         OpenBracket OpenParen,
                                         Ident "a",
                                         CloseBracket CloseParen,
                                         OpenBracket OpenBrace,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 1,
                                         SemiColon,
                                         CloseBracket CloseBrace,
                                         Keyword Else,
                                         OpenBracket OpenBrace,
                                         Ident "b",
                                         OpTok EqualSign,
                                         ConstInt 2,
                                         SemiColon,
                                         CloseBracket CloseBrace
                                        ])
                  `shouldBe`
                  ProgramNode [IfNode
                               (VarNode "a" mockNodeDat)
                               (CompoundStmtNode
                                [ExprStmtNode
                                 (AssignmentNode
                                  (VarNode "b" mockNodeDat)
                                  (ConstantNode 1 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                                mockNodeDat
                               )
                               (Just $ CompoundStmtNode
                                [ExprStmtNode
                                 (AssignmentNode
                                  (VarNode "b" mockNodeDat)
                                  (ConstantNode 2 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                                mockNodeDat
                               )
                               mockNodeDat
                              ]
