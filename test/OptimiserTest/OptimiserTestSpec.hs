
module OptimiserTest.OptimiserTestSpec (optimiserTest) where

import Test.Hspec

import Optimiser.Optimiser  (optimiseExpression)
import Types.AssemblySchema
import Types.Operator


optimiserTest :: IO ()
optimiserTest = hspec $ do
        describe "Optimise expressions to reduce code size" $ do

                it "Should return a literal schema unchanged" $
                  optimiseExpression (LiteralSchema 200)
                  `shouldBe`
                  (LiteralSchema 200)

                it "Should optimise a binary plus schema to a literal" $
                  optimiseExpression (buildBinarySchema Plus 20 2)
                  `shouldBe`
                  (LiteralSchema 22)

                it "Should optimise a binary minus schema to a literal" $
                  optimiseExpression (buildBinarySchema Minus 20 2)
                  `shouldBe`
                  (LiteralSchema 18)

                it "Should optimise a binary multiply schema to a literal" $
                  optimiseExpression (buildBinarySchema Multiply 20 2)
                  `shouldBe`
                  (LiteralSchema 40)

                it "Should optimise a binary modulo schema to a literal" $
                  optimiseExpression (buildBinarySchema Modulo 27 10)
                  `shouldBe`
                  (LiteralSchema 7)

                it "Should optimise a binary divide schema to a literal" $
                  optimiseExpression (buildBinarySchema Divide 20 2)
                  `shouldBe`
                  (LiteralSchema 10)

                it "Should optimise binary schema with negative result" $
                  optimiseExpression (buildBinarySchema Minus 2 20)
                  `shouldBe`
                  (UnarySchema (LiteralSchema 18) (Unary Negate))

                it "Should optimise nested binary schema" $
                  optimiseExpression (buildNestedBinarySchema Plus Plus Plus 20 2)
                  `shouldBe`
                  (LiteralSchema 44)

                it "Should optimise nested binary schema with negative result" $
                  optimiseExpression (buildNestedBinarySchema Plus Minus Minus 2 20)
                  `shouldBe`
                  (BinarySchema
                   (UnarySchema (LiteralSchema 18) (Unary Negate))
                   (UnarySchema (LiteralSchema 18) (Unary Negate))
                   Plus
                   (LocalLabel 5)
                   (LocalLabel 6)
                  )


buildBinarySchema :: BinaryOp -> Int -> Int -> ExpressionSchema
buildBinarySchema binOp n m =
        (BinarySchema
         (LiteralSchema n)
         (LiteralSchema m)
         binOp
         (LocalLabel 1)
         (LocalLabel 2)
        )


buildNestedBinarySchema :: BinaryOp
                        -> BinaryOp
                        -> BinaryOp
                        -> Int
                        -> Int
                        -> ExpressionSchema
buildNestedBinarySchema binOp1 binOp2 binOp3 n m =
        (BinarySchema
         (buildBinarySchema binOp2 n m)
         (buildBinarySchema binOp3 n m)
         binOp1
         (LocalLabel 5)
         (LocalLabel 6)
        )
