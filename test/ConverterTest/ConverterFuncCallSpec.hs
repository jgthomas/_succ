
module ConverterTest.ConverterFuncCallSpec (converterFuncCallTest) where


import Test.Hspec

import ConverterTest.TestUtility (extractSchema)
import TestUtility               (mockNodeDat)
import Types.AssemblySchema
import Types.AST
import Types.Type


converterFuncCallTest :: IO ()
converterFuncCallTest = hspec $ do
        describe "Build assembly schemas for function calls" $ do

                it "Should build a schema for a repeat declaration" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "dog"
                                                [ParamNode
                                                 IntVar
                                                 (VarNode "a" mockNodeDat)
                                                 mockNodeDat
                                                ]
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "dog"
                                                [ParamNode
                                                 IntVar
                                                 (VarNode "b" mockNodeDat)
                                                 mockNodeDat
                                                ]
                                                Nothing
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema [SkipSchema, SkipSchema]

                it "Should build a schema for a function call" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "dog"
                                                []
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [ReturnNode
                                                  (FuncCallNode "dog" [] mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [SkipSchema,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ReturnSchema
                      (ExpressionSchema $ FunctionCallSchema
                       "dog"
                       []
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a function call with arguments" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "dog"
                                                [ParamNode
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
                                                (Just $ CompoundStmtNode
                                                 [ReturnNode
                                                  (FuncCallNode
                                                   "dog"
                                                   [ArgNode
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
                  `shouldBe`
                  ProgramSchema
                   [SkipSchema,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ReturnSchema
                      (ExpressionSchema $ FunctionCallSchema
                       "dog"
                       [(ExpressionSchema $ LiteralSchema 29)]
                      )
                     ]
                    )
                   ]
