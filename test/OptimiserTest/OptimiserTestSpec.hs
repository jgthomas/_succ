
module OptimiserTest.OptimiserTestSpec (optimiserTest) where


import OptimiserTest.OptimiserExpressionTestSpec (optimiserExpressionTest)


optimiserTest :: IO ()
optimiserTest = do
        optimiserExpressionTest

