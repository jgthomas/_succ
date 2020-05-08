
module Main where


--import CheckerSpec
--import GeneratorSpec
import LexerTest.LexerSpec   (lexerTest)
import ParserTest.ParserSpec (parserTest)


main :: IO ()
main = do
        lexerTest
        parserTest
        --checkerTest
        --generatorTest
