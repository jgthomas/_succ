
module ConverterTest.ConverterTestSpec (converterTest) where


import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility               (mockNodeDat)
import Types.AssemblySchema
import Types.AST
import Types.Operator
import Types.Type
import Types.Variables


converterTest :: IO ()
converterTest = hspec $ do
        describe "Build assembly schemas from syntax trees" $ do

                it "Should create a global declaration schema" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    SkipSchema
                    Global
                    IntVar
                   ]

                it "Should create a global declaration schema with assignment" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "a" mockNodeDat)
                                     (ConstantNode 10 mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    (StatementSchema $ AssignmentSchema
                     (VariableSchema (GlobalVar "_a1" 0))
                     (LiteralSchema 10)
                     Global
                    )
                    Global
                    IntVar
                   ]

                it "Should create a global pointer declaration schema" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntPointer
                                    Nothing
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    SkipSchema
                    Global
                    IntPointer
                   ]

                it "Should create a function schema with local declaration" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-8) 0 0))
                     SkipSchema
                     Local
                     IntVar,
                     StatementSchema $ ReturnSchema (LiteralSchema 190)
                    ]
                   )
                  ]

                it "Should create a function schema with local declaration with assignment" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
                                     (Just $ AssignmentNode
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-8) 0 0))
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema (LocalVar (-8) 0 0))
                      (LiteralSchema 100)
                      Local
                     )
                     Local
                     IntVar,
                     StatementSchema $ ReturnSchema
                     (VariableSchema (LocalVar (-8) 0 0))
                    ]
                   )
                  ]
