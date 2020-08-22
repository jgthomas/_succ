
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
                  map (testBool Equal) boolTestData
                  `shouldBe`
                  [1, 1, 0, 0, 0, 0, 0]

                it "Should compute not equal" $
                  map (testBool NotEqual) boolTestData
                  `shouldBe`
                  [0, 0, 1, 1, 1, 1, 1]

                it "Should compute greater than" $
                  map (testBool GreaterThan) boolTestData
                  `shouldBe`
                  [0, 0, 1, 1, 0, 0, 1]

                it "Should compute less than" $
                  map (testBool LessThan) boolTestData
                  `shouldBe`
                  [0, 0, 0, 0, 1, 1, 0]

                it "Should compute greater than or equal" $
                  map (testBool GThanOrEqu) boolTestData
                  `shouldBe`
                  [1, 1, 1, 1, 0, 0, 1]

                it "Should compute less than or equal" $
                  map (testBool LThanOrEqu) boolTestData
                  `shouldBe`
                  [1, 1, 0, 0, 1, 1, 0]

                it "Should compute logical OR" $
                  map (testBool LogicalOR) boolTestData
                  `shouldBe`
                  [1, 0, 1, 1, 1, 1, 1]

                it "Should compute logical AND" $
                  map (testBool LogicalAND) boolTestData
                  `shouldBe`
                  [1, 0, 1, 1, 1, 0, 0]


testBool :: (Bits a, Integral a) => BinaryOp -> (a, a) -> a
testBool op pair = binaryFunction op (fst pair) (snd pair)


boolTestData :: [(Int, Int)]
boolTestData = [(3, 3),
                (0, 0),
                (13, 3),
                (13, -3),
                (3, 10),
                (0, 1),
                (1, 0)
               ]
