
module OptimiserTest.OptimiserStatementTestSpec (optimiserStatementTest) where


import Test.Hspec

import Optimiser.Optimiser  (optimise)
import Types.AssemblySchema


optimiserStatementTest :: IO ()
optimiserStatementTest = hspec $ do
        describe "Optimise statements to reduce code size" $ do

                it "Should return a statement schema unchanged" $
                  optimise (StatementSchema $ ReturnSchema SkipSchema)
                  `shouldBe`
                  StatementSchema (ReturnSchema SkipSchema)
