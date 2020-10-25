
module Main where


import ComputeTest.ComputeTestSpec     (computeTest)
import ConverterTest.ConverterTestSpec (converterTest)
import LexerTest.LexerTestSpec         (lexerTest)
import OptimiserTest.OptimiserTestSpec (optimiserTest)
import ParserTest.ParserTestSpec       (parserTest)


main :: IO ()
main = do
        computeTest
        lexerTest
        parserTest
        converterTest
        optimiserTest
