module ConverterTest.ConverterFuncCallSpec
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

spec :: Spec
spec = do
  describe "Build assembly schemas for function calls" $ do
    it "Should build a schema for a repeat declaration" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "dog"
                [ ParamNode
                    IntVar
                    (VarNode "a" mockNodeDat)
                    mockNodeDat
                ]
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "dog"
                [ ParamNode
                    IntVar
                    (VarNode "b" mockNodeDat)
                    mockNodeDat
                ]
                Nothing
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema [SkipSchema, SkipSchema]
    it "Should build a schema for a function call" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "dog"
                []
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ ReturnNode
                          (FuncCallNode "dog" [] mockNodeDat)
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ SkipSchema,
            FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        ReturnSchema
                          ( ExpressionSchema $
                              FunctionCallSchema
                                "dog"
                                []
                          )
                    ]
              )
          ]
    it "Should build a schema for a function call with arguments" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "dog"
                [ ParamNode
                    IntVar
                    (VarNode "a" mockNodeDat)
                    mockNodeDat
                ]
                Nothing
                mockNodeDat,
              FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ ReturnNode
                          ( FuncCallNode
                              "dog"
                              [ ArgNode
                                  (ConstantNode 29 mockNodeDat)
                                  mockNodeDat
                              ]
                              mockNodeDat
                          )
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ SkipSchema,
            FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ StatementSchema $
                        ReturnSchema
                          ( ExpressionSchema $
                              FunctionCallSchema
                                "dog"
                                [(ExpressionSchema $ LiteralSchema 29)]
                          )
                    ]
              )
          ]
    it "Should build a schema for a main function with no statements" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                (Just $ CompoundStmtNode [] mockNodeDat)
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema
                  ( CompoundStatementSchema
                      [StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 0)]
                  )
              )
          ]
    it "Should build a schema for a main function with explicit return statement" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ ExprStmtNode
                          ( BinaryNode
                              (ConstantNode 10 mockNodeDat)
                              (ConstantNode 10 mockNodeDat)
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
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema
                  ( CompoundStatementSchema
                      [ ExpressionSchema
                          ( BinarySchema
                              (ExpressionSchema (LiteralSchema 10))
                              (ExpressionSchema (LiteralSchema 10))
                              Plus
                              (LocalLabel 1)
                              (LocalLabel 2)
                          ),
                        StatementSchema
                          (ReturnSchema (ExpressionSchema $ LiteralSchema 0))
                      ]
                  )
              )
          ]
