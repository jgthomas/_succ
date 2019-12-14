
module Main where


import LexerSpec
import ParserSpec
import GeneratorSpec


main :: IO ()
main = do
        lexerTest
        parserTest
        generatorTest
