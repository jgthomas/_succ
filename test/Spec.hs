
module Main where


import CheckerTest.CheckerTestSpec     (checkerTest)
import GeneratorTest.GeneratorTestSpec (generatorTest)
import LexerTest.LexerTestSpec         (lexerTest)
import ParserTest.ParserTestSpec       (parserTest)


main :: IO ()
main = do
        lexerTest
        parserTest
        checkerTest
        generatorTest
