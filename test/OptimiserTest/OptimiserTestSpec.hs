
module OptimiserTest.OptimiserTestSpec (optimiserTest) where


import OptimiserTest.OptimiserExpressionTestSpec (optimiserExpressionTest)
import OptimiserTest.OptimiserStatementTestSpec  (optimiserStatementTest)
import OptimiserTest.OptimiserTopLevelTestSpec   (optimiserTopLevelTest)


optimiserTest :: IO ()
optimiserTest = do
        optimiserExpressionTest
        optimiserStatementTest
        optimiserTopLevelTest

