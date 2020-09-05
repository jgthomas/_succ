
module ParserTest.ParserStatementSpec (parserStatementTest) where


import Test.Hspec

import ParserTest.TestUtility (extractStatementError, extractStatementTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Error
import Types.Operator
import Types.Tokens
import Types.Type


parserStatementTest :: IO ()
parserStatementTest = hspec $ do
        describe "Build abstract syntax trees for statements" $ do

                it "Should build a tree for a continue statement" $
                  (extractStatementTree [Keyword Continue dummyLexDat, SemiColon dummyLexDat])
                  `shouldBe`
                  ProgramNode [ContinueNode mockNodeDat]

                it "Should build a tree for a break statement" $
                  (extractStatementTree [Keyword Break dummyLexDat, SemiColon dummyLexDat])
                  `shouldBe`
                  ProgramNode [BreakNode mockNodeDat]

                it "Should build a tree for a return statement" $
                  (extractStatementTree [Keyword Return dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat])
                  `shouldBe`
                  ProgramNode [ReturnNode
                               (ConstantNode 2 mockNodeDat)
                               mockNodeDat
                              ]

                it "Should build a tree for a null statement" $
                  (extractStatementTree [SemiColon dummyLexDat])
                  `shouldBe`
                  ProgramNode [NullExprNode mockNodeDat]

                it "Should build a tree for a simple compound statement" $
                  (extractStatementTree [OpenBracket OpenBrace dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         OpTok PlusSign dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         Keyword Break dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat
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
                  (extractStatementTree [OpenBracket OpenBrace dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         OpTok PlusSign dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         Keyword Int dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 3 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat
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
                  (extractStatementTree [Keyword If dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat
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
                  (extractStatementTree [Keyword If dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat
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
                  (extractStatementTree [Keyword If dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         Keyword Else dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat
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
                  (extractStatementTree [Keyword If dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat,
                                         Keyword Else dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "b" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat
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

                it "Should build a tree for a do-while loop" $
                  (extractStatementTree [Keyword Do dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok PlusEqual dummyLexDat,
                                         ConstInt 2 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat,
                                         Keyword While dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok LeftArrowEqual dummyLexDat,
                                         ConstInt 10 dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         SemiColon dummyLexDat]
                  )
                  `shouldBe`
                  ProgramNode [DoWhileNode
                               (CompoundStmtNode
                                [(ExprStmtNode
                                  (AssignmentNode
                                   (VarNode "a" mockNodeDat)
                                   (ConstantNode 2 mockNodeDat)
                                   (BinaryOp Plus)
                                   mockNodeDat
                                 )
                                 mockNodeDat
                                 )
                                ]
                                mockNodeDat
                               )
                               (BinaryNode
                                (VarNode "a" mockNodeDat)
                                (ConstantNode 10 mockNodeDat)
                                LThanOrEqu
                                mockNodeDat
                               )
                               mockNodeDat
                              ]

                it "Should build a tree for a while loop" $
                  (extractStatementTree [Keyword While dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok LeftArrow dummyLexDat,
                                         ConstInt 3 dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok PlusEqual dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat]
                  )
                  `shouldBe`
                  ProgramNode [WhileNode
                               (BinaryNode
                                (VarNode "a" mockNodeDat)
                                (ConstantNode 3 mockNodeDat)
                                LessThan
                                mockNodeDat
                               )
                               (CompoundStmtNode
                                [ExprStmtNode
                                 (AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  (ConstantNode 1 mockNodeDat)
                                  (BinaryOp Plus)
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                                mockNodeDat
                               )
                               mockNodeDat
                              ]

                it "Should build a tree for a conventional for loop" $
                  (extractStatementTree [Keyword For dummyLexDat,
                                         OpenBracket OpenParen dummyLexDat,
                                         Keyword Int dummyLexDat,
                                         Ident "i" dummyLexDat,
                                         OpTok EqualSign dummyLexDat,
                                         ConstInt 0 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         Ident "i" dummyLexDat,
                                         OpTok LeftArrow dummyLexDat,
                                         ConstInt 10 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         Ident "i" dummyLexDat,
                                         OpTok PlusPlus dummyLexDat,
                                         CloseBracket CloseParen dummyLexDat,
                                         OpenBracket OpenBrace dummyLexDat,
                                         Ident "a" dummyLexDat,
                                         OpTok PlusEqual dummyLexDat,
                                         ConstInt 1 dummyLexDat,
                                         SemiColon dummyLexDat,
                                         CloseBracket CloseBrace dummyLexDat
                                        ]
                  )
                  `shouldBe`
                  ProgramNode [ForLoopNode
                               (DeclarationNode
                                (VarNode "i" mockNodeDat)
                                IntVar
                                (Just $ AssignmentNode
                                 (VarNode "i" mockNodeDat)
                                 (ConstantNode 0 mockNodeDat)
                                 Assignment
                                 mockNodeDat
                                )
                                mockNodeDat
                               )
                               (ExprStmtNode
                                (BinaryNode
                                 (VarNode "i" mockNodeDat)
                                 (ConstantNode 10 mockNodeDat)
                                 LessThan
                                 mockNodeDat
                                )
                                mockNodeDat
                               )
                               (UnaryNode
                                (VarNode "i" mockNodeDat)
                                (PostOpUnary PostIncrement)
                                mockNodeDat
                               )
                               (CompoundStmtNode
                                [ExprStmtNode
                                 (AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  (ConstantNode 1 mockNodeDat)
                                  (BinaryOp Plus)
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                                mockNodeDat
                               )
                               mockNodeDat
                              ]

        describe "Throw errors on bad input" $ do

                it "Should throw error for do-while loop missing while keyword" $
                  let toks = [Keyword Do dummyLexDat,
                              OpenBracket OpenBrace dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok PlusEqual dummyLexDat,
                              ConstInt 2 dummyLexDat,
                              SemiColon dummyLexDat,
                              CloseBracket CloseBrace dummyLexDat,
                              SemiColon dummyLexDat,
                              OpenBracket OpenParen dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok LeftArrowEqual dummyLexDat,
                              ConstInt 10 dummyLexDat,
                              CloseBracket CloseParen dummyLexDat,
                              SemiColon dummyLexDat]
                      in
                  (extractStatementError toks)
                  `shouldBe`
                  SyntaxError (MissingKeyword While $ OpenBracket OpenParen dummyLexDat)

                it "Should throw error for do-while loop missing while open paren" $
                  let toks = [Keyword Do dummyLexDat,
                              OpenBracket OpenBrace dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok PlusEqual dummyLexDat,
                              ConstInt 2 dummyLexDat,
                              SemiColon dummyLexDat,
                              CloseBracket CloseBrace dummyLexDat,
                              Keyword While dummyLexDat,
                              SemiColon dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok LeftArrowEqual dummyLexDat,
                              ConstInt 10 dummyLexDat,
                              CloseBracket CloseParen dummyLexDat,
                              SemiColon dummyLexDat]
                      in
                  (extractStatementError toks)
                  `shouldBe`
                  SyntaxError (MissingToken
                               (OpenBracket OpenParen dummyLexDat)
                               $ Keyword While dummyLexDat
                              )

                it "Should throw error for do-while loop missing semicolon after while" $
                  let toks = [Keyword Do dummyLexDat,
                              OpenBracket OpenBrace dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok PlusEqual dummyLexDat,
                              ConstInt 2 dummyLexDat,
                              SemiColon dummyLexDat,
                              CloseBracket CloseBrace dummyLexDat,
                              Keyword While dummyLexDat,
                              OpenBracket OpenParen dummyLexDat,
                              Ident "a" dummyLexDat,
                              OpTok LeftArrowEqual dummyLexDat,
                              ConstInt 10 dummyLexDat,
                              CloseBracket CloseParen dummyLexDat,
                              Colon dummyLexDat]
                      in
                  (extractStatementError toks)
                  `shouldBe`
                  SyntaxError (MissingToken (SemiColon dummyLexDat) (Colon dummyLexDat))
