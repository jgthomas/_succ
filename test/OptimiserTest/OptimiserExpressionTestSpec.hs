module OptimiserTest.OptimiserExpressionTestSpec
  ( spec,
  )
where

import Optimiser.Optimiser (optimise)
import Test.Hspec
import Types.AssemblySchema
import Types.Operator

spec :: Spec
spec = do
  describe "Optimise expressions to reduce code size" $ do
    it "Should return a literal schema unchanged" $
      optimise (ExpressionSchema $ LiteralSchema 200)
        `shouldBe` (ExpressionSchema $ LiteralSchema 200)
    it "Should return a unary schema unchanged" $
      optimise (ExpressionSchema $ UnarySchema (ExpressionSchema $ LiteralSchema 100) (Unary Negate))
        `shouldBe` ExpressionSchema (UnarySchema (ExpressionSchema $ LiteralSchema 100) (Unary Negate))
    it "Should optimise a binary plus schema to a literal" $
      optimise (buildBinarySchema Plus 20 2)
        `shouldBe` (ExpressionSchema $ LiteralSchema 22)
    it "Should optimise a binary minus schema to a literal" $
      optimise (buildBinarySchema Minus 20 2)
        `shouldBe` (ExpressionSchema $ LiteralSchema 18)
    it "Should optimise a binary multiply schema to a literal" $
      optimise (buildBinarySchema Multiply 20 2)
        `shouldBe` (ExpressionSchema $ LiteralSchema 40)
    it "Should optimise a binary modulo schema to a literal" $
      optimise (buildBinarySchema Modulo 27 10)
        `shouldBe` (ExpressionSchema $ LiteralSchema 7)
    it "Should optimise a binary divide schema to a literal" $
      optimise (buildBinarySchema Divide 20 2)
        `shouldBe` (ExpressionSchema $ LiteralSchema 10)
    it "Should optimise binary schema with negative result" $
      optimise (buildBinarySchema Minus 2 20)
        `shouldBe` ExpressionSchema (UnarySchema (ExpressionSchema $ LiteralSchema 18) (Unary Negate))
    it "Should optimise nested binary schema" $
      optimise (buildNestedBinarySchema Plus Plus Plus 20 2)
        `shouldBe` (ExpressionSchema $ LiteralSchema 44)
    it "Should optimise nested binary schema with negative result" $
      optimise (buildNestedBinarySchema Plus Minus Minus 2 20)
        `shouldBe` ExpressionSchema
          ( BinarySchema
              (ExpressionSchema (UnarySchema (ExpressionSchema $ LiteralSchema 18) (Unary Negate)))
              (ExpressionSchema (UnarySchema (ExpressionSchema $ LiteralSchema 18) (Unary Negate)))
              Plus
              (LocalLabel 5)
              (LocalLabel 6)
          )
    it "Should pass back unchanged a binary schema with no optimisation support" $
      optimise (ExpressionSchema $ BinarySchema SkipSchema SkipSchema Plus (LocalLabel 1) (LocalLabel 2))
        `shouldBe` ExpressionSchema (BinarySchema SkipSchema SkipSchema Plus (LocalLabel 1) (LocalLabel 2))
    it "Should pass back unchanged a schema with no optimisation support" $
      optimise (ExpressionSchema $ DereferenceSchema SkipSchema)
        `shouldBe` ExpressionSchema (DereferenceSchema SkipSchema)

buildBinarySchema :: BinaryOp -> Int -> Int -> AssemblySchema
buildBinarySchema binOp n m =
  ExpressionSchema
    ( BinarySchema
        (ExpressionSchema $ LiteralSchema n)
        (ExpressionSchema $ LiteralSchema m)
        binOp
        (LocalLabel 1)
        (LocalLabel 2)
    )

buildNestedBinarySchema ::
  BinaryOp ->
  BinaryOp ->
  BinaryOp ->
  Int ->
  Int ->
  AssemblySchema
buildNestedBinarySchema binOp1 binOp2 binOp3 n m =
  ExpressionSchema
    ( BinarySchema
        (buildBinarySchema binOp2 n m)
        (buildBinarySchema binOp3 n m)
        binOp1
        (LocalLabel 5)
        (LocalLabel 6)
    )
