
module ComputeTest.ComputeTestSpec (computeTest) where


import Data.Bits
import Test.Hspec

import Compute.ComputeExpression
import Types.Operator


computeTest :: IO ()
computeTest = hspec $ do
        describe "Compute binary expressions" $ do

                it "Should compute addition" $
                  binaryFunction Plus (5 :: Int) (4 :: Int)
                  `shouldBe`
                  (9 :: Int)

                it "Should compute subtraction" $
                  binaryFunction Minus (5 :: Int) (4 :: Int)
                  `shouldBe`
                  (1 :: Int)

                it "Should compute multiplication" $
                  binaryFunction Multiply (5 :: Int) (4 :: Int)
                  `shouldBe`
                  (20 :: Int)

                it "Should compute division" $
                  binaryFunction Divide (10 :: Int) (2 :: Int)
                  `shouldBe`
                  (5 :: Int)

                it "Should compute modulo" $
                  binaryFunction Modulo (10 :: Int) (3 :: Int)
                  `shouldBe`
                  (1 :: Int)

                it "Should compute equal" $
                  map (testBin Equal) boolTestData
                  `shouldBe`
                  [1, 1, 0, 0, 0, 0, 0, 0]

                it "Should compute not equal" $
                  map (testBin NotEqual) boolTestData
                  `shouldBe`
                  [0, 0, 1, 1, 1, 1, 1, 1]

                it "Should compute greater than" $
                  map (testBin GreaterThan) boolTestData
                  `shouldBe`
                  [0, 0, 1, 1, 0, 0, 1, 1]

                it "Should compute less than" $
                  map (testBin LessThan) boolTestData
                  `shouldBe`
                  [0, 0, 0, 0, 1, 1, 0, 0]

                it "Should compute greater than or equal" $
                  map (testBin GThanOrEqu) boolTestData
                  `shouldBe`
                  [1, 1, 1, 1, 0, 0, 1, 1]

                it "Should compute less than or equal" $
                  map (testBin LThanOrEqu) boolTestData
                  `shouldBe`
                  [1, 1, 0, 0, 1, 1, 0, 0]

                it "Should compute logical OR" $
                  map (testBin LogicalOR) boolTestData
                  `shouldBe`
                  [1, 0, 1, 1, 1, 1, 1, 1]

                it "Should compute logical AND" $
                  map (testBin LogicalAND) boolTestData
                  `shouldBe`
                  [1, 0, 1, 1, 1, 0, 0, 1]

                it "Should compute bitwise XOR" $
                  map (testBin BitwiseXOR) boolTestData
                  `shouldBe`
                  [0, 0, 15, -16, 9, 1, 1, 33]

                it "Should compute bitwise AND" $
                  map (testBin BitwiseAND) boolTestData
                  `shouldBe`
                  [3, 0, 0, 13, 2, 0, 0, 0]

                it "Should compute bitwise OR" $
                  map (testBin BitwiseOR) boolTestData
                  `shouldBe`
                  [3, 0, 15, -3, 11, 1, 1, 33]

                it "Should compute left shift" $
                  map (testBin (ShiftOp LeftShift)) shiftTestData
                  `shouldBe`
                  [24, 0, 52, 104, 3072, 0, 1, 64]

                it "Should compute right shift" $
                  map (testBin (ShiftOp RightShift)) shiftTestData
                  `shouldBe`
                  [0, 0, 3, 1, 0, 0, 1, 16]


testBin :: (Bits a, Integral a) => BinaryOp -> (a, a) -> a
testBin op pair = binaryFunction op (fst pair) (snd pair)


boolTestData :: [(Int, Int)]
boolTestData = [(3, 3),
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
