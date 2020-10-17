module ConverterTest.ConverterStatementSpec
  ( converterStatementTest,
  )
where

import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility (mockNodeDat)
import Types.AST
import Types.AssemblySchema
import Types.Operator
import Types.Type
import Types.Variables

converterStatementTest :: IO ()
converterStatementTest = hspec $ do
  describe "Build assembly schemas for loops and conditionals" $ do
    it "Should build a schema for a basic if statement" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ IfNode
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 90 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          ( ReturnNode
                              (ConstantNode 22 mockNodeDat)
                              mockNodeDat
                          )
                          Nothing
                          mockNodeDat,
                        ReturnNode
                          (ConstantNode 66 mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        IfSchema
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ LiteralSchema 10)
                                (ExpressionSchema $ LiteralSchema 90)
                                LessThan
                                (LocalLabel 3)
                                (LocalLabel 4)
                          )
                          ( StatementSchema $
                              ReturnSchema
                                (ExpressionSchema $ LiteralSchema 22)
                          )
                          SkipSchema
                          (LocalLabel 1)
                          (LocalLabel 2),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ LiteralSchema 66)
                      )
                    ]
              )
          ]
    it "Should build a schema for a while statement" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ WhileNode
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 90 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          ( CompoundStmtNode
                              [ AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  (ConstantNode 100 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                              ]
                              mockNodeDat
                          )
                          mockNodeDat,
                        ReturnNode
                          (VarNode "a" mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema,
            FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        WhileSchema
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ LiteralSchema 10)
                                (ExpressionSchema $ LiteralSchema 90)
                                LessThan
                                (LocalLabel 4)
                                (LocalLabel 5)
                          )
                          ( StatementSchema $
                              CompoundStatementSchema
                                [ StatementSchema $
                                    AssignmentSchema
                                      (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                                      (ExpressionSchema $ LiteralSchema 100)
                                      Local
                                ]
                          )
                          (LocalLabel 2)
                          (LocalLabel 3),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) UntrackedValue)
                      )
                    ]
              )
          ]
    it "Should build a schema for a do-while statement" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ DoWhileNode
                          ( CompoundStmtNode
                              [ AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  (ConstantNode 100 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                              ]
                              mockNodeDat
                          )
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 90 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          mockNodeDat,
                        ReturnNode
                          (VarNode "a" mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema,
            FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        DoWhileSchema
                          ( StatementSchema $
                              CompoundStatementSchema
                                [ StatementSchema $
                                    AssignmentSchema
                                      (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                                      (ExpressionSchema $ LiteralSchema 100)
                                      Local
                                ]
                          )
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ LiteralSchema 10)
                                (ExpressionSchema $ LiteralSchema 90)
                                LessThan
                                (LocalLabel 5)
                                (LocalLabel 6)
                          )
                          (LocalLabel 2)
                          (LocalLabel 3)
                          (LocalLabel 4),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) UntrackedValue)
                      )
                    ]
              )
          ]
    it "Should build a schema for a for statement" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ ForLoopNode
                          ( DeclarationNode
                              (VarNode "i" mockNodeDat)
                              IntVar
                              ( Just $
                                  AssignmentNode
                                    (VarNode "i" mockNodeDat)
                                    (ConstantNode 1 mockNodeDat)
                                    Assignment
                                    mockNodeDat
                              )
                              mockNodeDat
                          )
                          ( BinaryNode
                              (VarNode "i" mockNodeDat)
                              (ConstantNode 10 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          ( BinaryNode
                              (VarNode "i" mockNodeDat)
                              (ConstantNode 1 mockNodeDat)
                              Plus
                              mockNodeDat
                          )
                          ( CompoundStmtNode
                              [ AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  (ConstantNode 100 mockNodeDat)
                                  Assignment
                                  mockNodeDat
                              ]
                              mockNodeDat
                          )
                          mockNodeDat,
                        ReturnNode
                          (VarNode "a" mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema,
            FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        ForSchema
                          ( DeclarationSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                              ( StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 1)
                                    Local
                              )
                              Local
                              IntVar
                          )
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                (ExpressionSchema $ LiteralSchema 10)
                                LessThan
                                (LocalLabel 5)
                                (LocalLabel 6)
                          )
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                (ExpressionSchema $ LiteralSchema 1)
                                Plus
                                (LocalLabel 7)
                                (LocalLabel 8)
                          )
                          ( StatementSchema $
                              CompoundStatementSchema
                                [ StatementSchema $
                                    AssignmentSchema
                                      (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                                      (ExpressionSchema $ LiteralSchema 100)
                                      Local
                                ]
                          )
                          (LocalLabel 2)
                          (LocalLabel 3)
                          (LocalLabel 4),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) UntrackedValue)
                      )
                    ]
              )
          ]
    it "Should build a schema for a loop with a break" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ WhileNode
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 90 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          ( CompoundStmtNode
                              [BreakNode mockNodeDat]
                              mockNodeDat
                          )
                          mockNodeDat,
                        ReturnNode
                          (ConstantNode 10 mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        WhileSchema
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ LiteralSchema 10)
                                (ExpressionSchema $ LiteralSchema 90)
                                LessThan
                                (LocalLabel 3)
                                (LocalLabel 4)
                          )
                          ( StatementSchema $
                              CompoundStatementSchema
                                [StatementSchema $ BreakSchema (LocalLabel 2)]
                          )
                          (LocalLabel 1)
                          (LocalLabel 2),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ LiteralSchema 10)
                      )
                    ]
              )
          ]
    it "Should build a schema for a loop with a continue" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ WhileNode
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 90 mockNodeDat)
                              LessThan
                              mockNodeDat
                          )
                          ( CompoundStmtNode
                              [ContinueNode mockNodeDat]
                              mockNodeDat
                          )
                          mockNodeDat,
                        ReturnNode
                          (ConstantNode 10 mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        WhileSchema
                          ( ExpressionSchema $
                              BinarySchema
                                (ExpressionSchema $ LiteralSchema 10)
                                (ExpressionSchema $ LiteralSchema 90)
                                LessThan
                                (LocalLabel 3)
                                (LocalLabel 4)
                          )
                          ( StatementSchema $
                              CompoundStatementSchema
                                [StatementSchema $ ContinueSchema (LocalLabel 1)]
                          )
                          (LocalLabel 1)
                          (LocalLabel 2),
                      ( StatementSchema $
                          ReturnSchema
                            (ExpressionSchema $ LiteralSchema 10)
                      )
                    ]
              )
          ]
