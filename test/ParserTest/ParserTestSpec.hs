module ParserTest.ParserTestSpec where

import ParserTest.ParserDeclarationSpec (parserDeclarationTest)
import ParserTest.ParserExpressionSpec (parserExpressionTest)
import ParserTest.ParserFunctionSpec (parserFunctionTest)
import ParserTest.ParserSpec (fullParserTest)
import ParserTest.ParserStatementSpec (parserStatementTest)
import ParserTest.TokClassSpec (tokClassTest)

parserTest :: IO ()
parserTest = do
  tokClassTest
  parserExpressionTest
  parserStatementTest
  parserDeclarationTest
  parserFunctionTest
  fullParserTest
