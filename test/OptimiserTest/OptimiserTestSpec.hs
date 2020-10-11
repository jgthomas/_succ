
module OptimiserTest.OptimiserTestSpec (optimiserTest) where


import OptimiserTest.OptimiserExpressionTestSpec (optimiserExpressionTest)
import OptimiserTest.OptimiserStatementTestSpec  (optimiserStatementTest)


optimiserTest :: IO ()
optimiserTest = do
        optimiserExpressionTest
        optimiserStatementTest

