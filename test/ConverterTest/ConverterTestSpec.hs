
module ConverterTest.ConverterTestSpec (converterTest) where


import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility               (mockNodeDat)
import Types.AssemblySchema
import Types.AST


converterTest :: IO ()
converterTest = hspec $ do
        describe "Test converter" $ do

                it "Should create a constant schema" $
                  (extractSchema $ (ConstantNode 2 mockNodeDat))
                  `shouldBe`
                  ExpressionSchema (Literal 2)

                it "Should create a variable schema" $
                  (extractSchema $ (VarNode "a" mockNodeDat))
                  `shouldBe`
                  ExpressionSchema (Variable "a")

                it "Should create a return statement schema" $
                  (extractSchema $ (ReturnNode
                                    (ConstantNode 2 mockNodeDat)
                                    mockNodeDat
                                   )
                  )
                  `shouldBe`
                  StatementSchema (ReturnSchema (Literal 2))
