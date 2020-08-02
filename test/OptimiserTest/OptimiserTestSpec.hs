
module OptimiserTest.OptimiserTestSpec (optimiserTest) where

import Test.Hspec

import Optimiser.Optimiser  (optimiseExpression)
import Types.AssemblySchema
import Types.Operator


optimiserTest :: IO ()
optimiserTest = hspec $ do
        describe "Optimise expressions to reduce code size" $ do

                it "Should optimise a binary plus schema to a literal" $
                  optimiseExpression (buildBinarySchema Plus)
                  `shouldBe`
                  (LiteralSchema 22)

                it "Should optimise a binary minus schema to a literal" $
                  optimiseExpression (buildBinarySchema Minus)
                  `shouldBe`
                  (LiteralSchema 18)

                it "Should optimise a binary multiply schema to a literal" $
                  optimiseExpression (buildBinarySchema Multiply)
                  `shouldBe`
                  (LiteralSchema 40)

                it "Should optimise a binary divide schema to a literal" $
                  optimiseExpression (buildBinarySchema Divide)
                  `shouldBe`
                  (LiteralSchema 10)


buildBinarySchema :: BinaryOp -> ExpressionSchema
buildBinarySchema binOp =
        (BinarySchema
         (LiteralSchema 20)
         (LiteralSchema 2)
         binOp
         (LocalLabel 1)
         (LocalLabel 2)
        )
