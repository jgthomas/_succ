module OptimiserTest.OptimiserTopLevelTestSpec
  ( optimiserTopLevelTest,
  )
where

import Optimiser.Optimiser (optimise)
import Test.Hspec
import Types.AssemblySchema
import Types.Type (Type (..))
import Types.Variables (Scope (..))

optimiserTopLevelTest :: IO ()
optimiserTopLevelTest = hspec $ do
  describe "Optimise top level schema to reduce code size" $ do
    it "Should return a program schema unchanged" $
      optimise (ProgramSchema [])
        `shouldBe` ProgramSchema []
    it "Should return a declaration schema unchanged" $
      optimise (DeclarationSchema SkipSchema SkipSchema Global IntVar)
        `shouldBe` DeclarationSchema SkipSchema SkipSchema Global IntVar
    it "Should return a function schema unchanged" $
      optimise (FunctionSchema "main" SkipSchema)
        `shouldBe` FunctionSchema "main" SkipSchema
    it "Should return a skipschema unchanged" $
      optimise SkipSchema
        `shouldBe` SkipSchema
