
module LexerTest.LexerTestSpec (lexerTest) where


import LexerTest.LexerTokensTestSpec (lexerTokensTest)


lexerTest :: IO ()
lexerTest = do
        lexerTokensTest
