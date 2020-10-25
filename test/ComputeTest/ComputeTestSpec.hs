module ComputeTest.ComputeTestSpec
  ( spec,
  )
where

import Compute.ComputeExpression
import Data.Bits
import Test.Hspec
import Types.Operator

spec :: Spec
spec = do
  describe "Compute binary expressions" $ do
    it "Should compute addition" $
      binaryFunction Plus (5 :: Int) (4 :: Int)
        `shouldBe` (9 :: Int)
    it "Should compute subtraction" $
      binaryFunction Minus (5 :: Int) (4 :: Int)
        `shouldBe` (1 :: Int)
    it "Should compute multiplication" $
      binaryFunction Multiply (5 :: Int) (4 :: Int)
        `shouldBe` (20 :: Int)
    it "Should compute division" $
      binaryFunction Divide (10 :: Int) (2 :: Int)
        `shouldBe` (5 :: Int)
    it "Should compute modulo" $
      binaryFunction Modulo (10 :: Int) (3 :: Int)
        `shouldBe` (1 :: Int)
    it "Should compute equal" $
      map (testBin Equal) boolTestData
        `shouldBe` [1, 1, 0, 0, 0, 0, 0, 0]
    it "Should compute not equal" $
      map (testBin NotEqual) boolTestData
        `shouldBe` [0, 0, 1, 1, 1, 1, 1, 1]
    it "Should compute greater than" $
      map (testBin GreaterThan) boolTestData
        `shouldBe` [0, 0, 1, 1, 0, 0, 1, 1]
    it "Should compute less than" $
      map (testBin LessThan) boolTestData
        `shouldBe` [0, 0, 0, 0, 1, 1, 0, 0]
    it "Should compute greater than or equal" $
      map (testBin GThanOrEqu) boolTestData
        `shouldBe` [1, 1, 1, 1, 0, 0, 1, 1]
    it "Should compute less than or equal" $
      map (testBin LThanOrEqu) boolTestData
        `shouldBe` [1, 1, 0, 0, 1, 1, 0, 0]
    it "Should compute logical OR" $
      map (testBin LogicalOR) boolTestData
        `shouldBe` [1, 0, 1, 1, 1, 1, 1, 1]
    it "Should compute logical AND" $
      map (testBin LogicalAND) boolTestData
        `shouldBe` [1, 0, 1, 1, 1, 0, 0, 1]
    it "Should compute bitwise XOR" $
      map (testBin BitwiseXOR) boolTestData
        `shouldBe` [0, 0, 15, -16, 9, 1, 1, 33]
    it "Should compute bitwise AND" $
      map (testBin BitwiseAND) boolTestData
        `shouldBe` [3, 0, 0, 13, 2, 0, 0, 0]
    it "Should compute bitwise OR" $
      map (testBin BitwiseOR) boolTestData
        `shouldBe` [3, 0, 15, -3, 11, 1, 1, 33]
    it "Should compute left shift" $
      map (testBin (ShiftOp LeftShift)) shiftTestData
        `shouldBe` [24, 0, 52, 104, 3072, 0, 1, 64]
    it "Should compute right shift" $
      map (testBin (ShiftOp RightShift)) shiftTestData
        `shouldBe` [0, 0, 3, 1, 0, 0, 1, 16]
  describe "Compute unary expressions" $ do
    it "Should compute unary minus" $
      map (unaryFunction $ Unary Negate) unaryTestData
        `shouldBe` [-1, 0, 1, -100, -2, -3, -99, 100]
    it "Should compute unary plus" $
      map (unaryFunction $ Unary Positive) unaryTestData
        `shouldBe` [1, 0, -1, 100, 2, 3, 99, -100]
    it "Should compute bitwise complement" $
      map (unaryFunction $ Unary BitwiseComp) unaryTestData
        `shouldBe` [-2, -1, 0, -101, -3, -4, -100, 99]
    it "Should compute logic negation" $
      map (unaryFunction $ Unary LogicalNeg) unaryTestData
        `shouldBe` [0, 1, 0, 0, 0, 0, 0, 0]
    it "Should compute pre-increment" $
      map (unaryFunction $ PreOpUnary PreIncrement) unaryTestData
        `shouldBe` [2, 1, 0, 101, 3, 4, 100, -99]
    it "Should compute pre-decrement" $
      map (unaryFunction $ PreOpUnary PreDecrement) unaryTestData
        `shouldBe` [0, -1, -2, 99, 1, 2, 98, -101]
    it "Should compute post-increment" $
      map (unaryFunction $ PostOpUnary PostIncrement) unaryTestData
        `shouldBe` [2, 1, 0, 101, 3, 4, 100, -99]
    it "Should compute post-decrement" $
      map (unaryFunction $ PostOpUnary PostDecrement) unaryTestData
        `shouldBe` [0, -1, -2, 99, 1, 2, 98, -101]
  describe "Compute literal bool values" $ do
    it "Should compute literal to bool" $
      map constantTrue [0, 1, -1, 100, 99, 55, -87]
        `shouldBe` [0, 1, 1, 1, 1, 1, 1]

unaryTestData :: [Int]
unaryTestData = [1, 0, -1, 100, 2, 3, 99, -100]

testBin :: (Bits a, Integral a) => BinaryOp -> (a, a) -> a
testBin op pair = binaryFunction op (fst pair) (snd pair)

boolTestData :: [(Int, Int)]
boolTestData =
  [ (3, 3),
    (0, 0),
    (13, 2),
    (13, -3),
    (3, 10),
    (0, 1),
    (1, 0),
    (32, 1)
  ]

shiftTestData :: [(Int, Int)]
shiftTestData = map (\(a, b) -> (abs a, abs b)) boolTestData
