
module LexerTest.LexerTestSpec (lexerTest) where


import LexerTest.LexerErrorTestSpec    (lexerErrorTest)
import LexerTest.LexerMetadataTestSpec (lexerMetadataTest)
import LexerTest.LexerTokensTestSpec   (lexerTokensTest)


lexerTest :: IO ()
lexerTest = do
        lexerTokensTest
        lexerMetadataTest
        lexerErrorTest
