
module Main where


import CheckerSpec
import GeneratorSpec
import LexerSpec
import ParserSpec


main :: IO ()
main = do
        lexerTest
        parserTest
        checkerTest
        generatorTest
