
module Main where


import CheckerTest.CheckerTestSpec     (checkerTest)
import ConverterTest.ConverterTestSpec (converterTest)
import LexerTest.LexerTestSpec         (lexerTest)
import ParserTest.ParserTestSpec       (parserTest)


main :: IO ()
main = do
        lexerTest
        parserTest
        converterTest
        checkerTest
