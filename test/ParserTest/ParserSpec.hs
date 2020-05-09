
module ParserTest.ParserSpec where


import ParserTest.ParserExpressionSpec (parserExpressionTest)
import ParserTest.TokClassSpec         (tokClassTest)


parserTest :: IO ()
parserTest = do
        parserExpressionTest
        tokClassTest
