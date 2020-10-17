module ConverterTest.ConverterExpressionSpec
  ( converterExpressionTest,
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

converterExpressionTest :: IO ()
converterExpressionTest = hspec $ do
  describe "Build assembly schemas for expressions" $ do
    it "Should create a schema for a function with local declaration" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ DeclarationNode
                            (VarNode "a" mockNodeDat)
                            IntVar
                            Nothing
                            mockNodeDat,
                          ReturnNode
                            (ConstantNode 190 mockNodeDat)
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        SkipSchema
                        Local
                        IntVar,
                      StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 190)
                    ]
              )
          ]
    it "Should create a schema for a function with an expression statement" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ExprStmtNode
                            ( BinaryNode
                                (ConstantNode 2 mockNodeDat)
                                (ConstantNode 2 mockNodeDat)
                                Plus
                                mockNodeDat
                            )
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ ExpressionSchema $
                        BinarySchema
                          (ExpressionSchema $ LiteralSchema 2)
                          (ExpressionSchema $ LiteralSchema 2)
                          Plus
                          (LocalLabel 1)
                          (LocalLabel 2),
                      StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 0)
                    ]
              )
          ]
    it "Should create a schema for a function with ternary operation" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ DeclarationNode
                            (VarNode "a" mockNodeDat)
                            IntVar
                            ( Just $
                                AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  ( TernaryNode
                                      ( BinaryNode
                                          (ConstantNode 12 mockNodeDat)
                                          (ConstantNode 10 mockNodeDat)
                                          GreaterThan
                                          mockNodeDat
                                      )
                                      (ConstantNode 90 mockNodeDat)
                                      (ConstantNode 100 mockNodeDat)
                                      mockNodeDat
                                  )
                                  Assignment
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
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            AssignmentSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                              ( ExpressionSchema $
                                  TernarySchema
                                    ( ExpressionSchema $
                                        BinarySchema
                                          (ExpressionSchema $ LiteralSchema 12)
                                          (ExpressionSchema $ LiteralSchema 10)
                                          GreaterThan
                                          (LocalLabel 1)
                                          (LocalLabel 2)
                                    )
                                    (ExpressionSchema $ LiteralSchema 90)
                                    (ExpressionSchema $ LiteralSchema 100)
                                    (LocalLabel 3)
                                    (LocalLabel 4)
                              )
                              Local
                        )
                        Local
                        IntVar,
                      StatementSchema
                        ( ReturnSchema
                            (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        )
                    ]
              )
          ]
    it "Should create a schema for a function with binary operation" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ DeclarationNode
                            (VarNode "a" mockNodeDat)
                            IntVar
                            ( Just $
                                AssignmentNode
                                  (VarNode "a" mockNodeDat)
                                  ( BinaryNode
                                      (ConstantNode 10 mockNodeDat)
                                      (ConstantNode 10 mockNodeDat)
                                      Plus
                                      mockNodeDat
                                  )
                                  Assignment
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
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            AssignmentSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                              ( ExpressionSchema $
                                  BinarySchema
                                    (ExpressionSchema $ LiteralSchema 10)
                                    (ExpressionSchema $ LiteralSchema 10)
                                    Plus
                                    (LocalLabel 1)
                                    (LocalLabel 2)
                              )
                              Local
                        )
                        Local
                        IntVar,
                      StatementSchema
                        ( ReturnSchema
                            (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) (SingleValue 20))
                        )
                    ]
              )
          ]
    it "Should create a schema for a function returning a unary negation" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ DeclarationNode
                            (VarNode "a" mockNodeDat)
                            IntVar
                            Nothing
                            mockNodeDat,
                          ReturnNode
                            ( UnaryNode
                                (VarNode "a" mockNodeDat)
                                (Unary Negate)
                                mockNodeDat
                            )
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        SkipSchema
                        Local
                        IntVar,
                      StatementSchema
                        ( ReturnSchema
                            ( ExpressionSchema $
                                UnarySchema
                                  (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                  (Unary Negate)
                            )
                        )
                    ]
              )
          ]
