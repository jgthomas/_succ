
module OptimiserTest.OptimiserTestSpec (optimiserTest) where

import Test.Hspec

import Optimiser.Optimiser  (optimiseExpression)
import Types.AssemblySchema
import Types.Operator


optimiserTest :: IO ()
optimiserTest = hspec $ do
        describe "Optimise expressions to reduce code size" $ do

                it "Should optimise a binary schema to a literal" $
                  optimiseExpression (BinarySchema
                                      (LiteralSchema 2)
                                      (LiteralSchema 2)
                                      Plus
                                      (LocalLabel 1)
                                      (LocalLabel 2)
                                     )
                  `shouldBe`
                  (LiteralSchema 4)
