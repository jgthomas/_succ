
module ConverterTest.ConverterTestSpec (converterTest) where


import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility               (mockNodeDat)
import Types.AssemblySchema
import Types.AST
import Types.Operator
import Types.Type


converterTest :: IO ()
converterTest = hspec $ do
        describe "Test converter" $ do

                it "Should create a constant schema" $
                  (extractSchema $ (ConstantNode 2 mockNodeDat))
                  `shouldBe`
                  ExpressionSchema (LiteralSchema 2)

                it "Should create a variable schema" $
                  (extractSchema $ (VarNode "a" mockNodeDat))
                  `shouldBe`
                  ExpressionSchema (VariableSchema "a")

                it "Should create a return statement schema" $
                  (extractSchema $ (ReturnNode
                                    (ConstantNode 2 mockNodeDat)
                                    mockNodeDat
                                   )
                  )
                  `shouldBe`
                  StatementSchema (ReturnSchema (LiteralSchema 2))

                it "Should create a unary schema" $
                  (extractSchema (UnaryNode
                                  (VarNode "a" mockNodeDat)
                                  (Unary Negate)
                                  mockNodeDat
                                 )
                  )
                  `shouldBe`
                  ExpressionSchema (UnarySchema
                                    (VariableSchema "a")
                                    (Unary Negate)
                                   )


                it "Should create a function schema" $
                  (extractSchema (FunctionNode
                                  IntVar
                                  "main"
                                  [(VarNode "a" mockNodeDat),
                                   ReturnNode
                                   (ConstantNode 2 mockNodeDat)
                                   mockNodeDat
                                  ]
                                  Nothing
                                  mockNodeDat
                                 )
                  )
                  `shouldBe`
                  FunctionSchema "main"
                                 [ExpressionSchema (VariableSchema "a"),
                                  StatementSchema (ReturnSchema (LiteralSchema 2))
                                 ]

                it "Should create a program schema" $
                  (extractSchema $ ProgramNode
                                [
                                 (FunctionNode
                                  IntVar
                                  "main"
                                  [(VarNode "a" mockNodeDat),
                                   ReturnNode
                                   (ConstantNode 2 mockNodeDat)
                                   mockNodeDat
                                  ]
                                  Nothing
                                  mockNodeDat
                                 ),
                                 (VarNode "a" mockNodeDat)
                                ]
                  )
                  `shouldBe`
                  ProgramSchema [FunctionSchema "main"
                                 [ExpressionSchema (VariableSchema "a"),
                                  StatementSchema (ReturnSchema (LiteralSchema 2))
                                 ],
                                 ExpressionSchema (VariableSchema "a")
                                ]
