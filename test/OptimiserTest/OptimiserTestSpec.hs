
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

                it "Should optimise nested binary schema" $
                  optimiseExpression (buildNestedBinarySchema Plus Plus Plus)
                  `shouldBe`
                  (LiteralSchema 44)

                it "Should optimise binary schema with negative number" $
                  optimiseExpression (BinarySchema
                                      (LiteralSchema 2)
                                      (LiteralSchema 20)
                                      Minus
                                      (LocalLabel 1)
                                      (LocalLabel 2)
                                     )
                  `shouldBe`
                  (UnarySchema (LiteralSchema 18) (Unary Negate))


buildBinarySchema :: BinaryOp -> ExpressionSchema
buildBinarySchema binOp =
        (BinarySchema
         (LiteralSchema 20)
         (LiteralSchema 2)
         binOp
         (LocalLabel 1)
         (LocalLabel 2)
        )


buildNestedBinarySchema :: BinaryOp -> BinaryOp -> BinaryOp -> ExpressionSchema
buildNestedBinarySchema binOp1 binOp2 binOp3 =
        (BinarySchema
         (buildBinarySchema binOp2)
         (buildBinarySchema binOp3)
         binOp1
         (LocalLabel 5)
         (LocalLabel 6)
        )
