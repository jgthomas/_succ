
module Main where


import GeneratorSpec
import LexerSpec
import ParserSpec


main :: IO ()
main = do
        lexerTest
        parserTest
        generatorTest
