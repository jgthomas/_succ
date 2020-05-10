
module ParserTest.ParserTestSpec where


import ParserTest.ParserDeclarationSpec (parserDeclarationTest)
import ParserTest.ParserExpressionSpec  (parserExpressionTest)
import ParserTest.ParserSpec            (fullParserTest)
import ParserTest.TokClassSpec          (tokClassTest)


parserTest :: IO ()
parserTest = do
        parserExpressionTest
        parserDeclarationTest
        tokClassTest
        fullParserTest
