module ConverterTest.ConverterDeclarationSpec
  ( spec,
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

spec :: Spec
spec = do
  describe "Build assembly schemas for declarations" $ do
    it "Should create a global declaration schema" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema
          ]
    it "Should create a schema for a global declaration with assignment" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                ( Just $
                    AssignmentNode
                      (VarNode "a" mockNodeDat)
                      (ConstantNode 10 mockNodeDat)
                      Assignment
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              ( StatementSchema $
                  AssignmentSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                    (ExpressionSchema $ LiteralSchema 10)
                    Global
              )
              Global
              IntVar
          ]
    it "Should create a schema for a function with local declaration with assignment" $
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
                                  (ConstantNode 100 mockNodeDat)
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
                              (ExpressionSchema $ LiteralSchema 100)
                              Local
                        )
                        Local
                        IntVar,
                      StatementSchema $
                        ReturnSchema
                          (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) (SingleValue 100))
                    ]
              )
          ]
    it "Should create a schema using SkipSchema for a function repeatedly declared" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "dog"
                []
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "dog"
                []
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "dog"
                []
                ( Just $
                    CompoundStmtNode
                      [ ReturnNode
                          (ConstantNode 10 mockNodeDat)
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
            SkipSchema,
            SkipSchema,
            FunctionSchema
              "dog"
              ( StatementSchema $
                  CompoundStatementSchema
                    [StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 10)]
              )
          ]
