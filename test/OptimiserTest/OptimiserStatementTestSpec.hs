module OptimiserTest.OptimiserStatementTestSpec
  ( spec,
  )
where

import Optimiser.Optimiser (optimise)
import Test.Hspec
import Types.AssemblySchema

spec :: Spec
spec = do
  describe "Optimise statements to reduce code size" $ do
    it "Should return a statement schema unchanged" $
      optimise (StatementSchema $ ReturnSchema SkipSchema)
        `shouldBe` StatementSchema (ReturnSchema SkipSchema)
