
module Main where


import CheckerTest.CheckerTestSpec     (checkerTest)
import ConverterTest.ConverterTestSpec (converterTest)
import LexerTest.LexerTestSpec         (lexerTest)
import OptimiserTest.OptimiserTestSpec (optimiserTest)
import ParserTest.ParserTestSpec       (parserTest)


main :: IO ()
main = do
        lexerTest
        parserTest
        checkerTest
        converterTest
        optimiserTest
