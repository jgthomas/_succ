
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
        describe "Test converter" $ do

                it "Should create a global declaration schema" $
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
                   ]

                it "Should create a function schema with local declaration and assignment" $
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
                     Local,
                     StatementSchema $ ReturnSchema
                     (VariableSchema (LocalVar (-8) 0 0))
                    ]
                   )
                  ]
