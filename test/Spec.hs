
module Main where


import CheckerTest.CheckerSpec     (checkerTest)
import GeneratorTest.GeneratorSpec (generatorTest)
import LexerTest.LexerSpec         (lexerTest)
import ParserTest.ParserSpec       (parserTest)


main :: IO ()
main = do
        lexerTest
        parserTest
        checkerTest
        generatorTest
