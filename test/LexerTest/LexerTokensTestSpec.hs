
module LexerTest.LexerTokensTestSpec (lexerTokensTest) where


import Data.Either
import Test.Hspec

import Lexer.Lexer
import LexerTest.TestUtility (dummyData)
import Types.Tokens


lexerTokensTest :: IO ()
lexerTokensTest = hspec $ do
        describe "Lex input string into tokens" $ do

                it "Should correctly lex all separator characters" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["(",
                   ")",
                   "{",
                   "}",
                   ";",
                   ":",
                   "?",
                   ",",
                   "[",
                   "]"])
                  `shouldBe`
                  [OpenBracket OpenParen dummyLexDat,
                   CloseBracket CloseParen dummyLexDat,
                   OpenBracket OpenBrace dummyLexDat,
                   CloseBracket CloseBrace dummyLexDat,
                   Separator SemiColon dummyLexDat,
                   Separator Colon dummyLexDat,
                   Separator QuestMark dummyLexDat,
                   Separator Comma dummyLexDat,
                   OpenBracket OpenSqBracket dummyLexDat,
                   CloseBracket CloseSqBracket dummyLexDat]

                it "Should correctly lex all language keywords" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["int",
                   "return",
                   "if",
                   "else",
                   "for",
                   "while",
                   "do",
                   "break",
                   "continue"])
                  `shouldBe`
                  [Keyword Int dummyLexDat,
                   Keyword Return dummyLexDat,
                   Keyword If dummyLexDat,
                   Keyword Else dummyLexDat,
                   Keyword For dummyLexDat,
                   Keyword While dummyLexDat,
                   Keyword Do dummyLexDat,
                   Keyword Break dummyLexDat,
                   Keyword Continue dummyLexDat]

                it "Should correctly lex valid identifiers" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["main",
                   "dog",
                   "_cat",
                   "Mouse"])
                  `shouldBe`
                  [Ident "main" dummyLexDat,
                   Ident "dog" dummyLexDat,
                   Ident "_cat" dummyLexDat,
                   Ident "Mouse" dummyLexDat]

                it "Should correctly lex all single-character operators" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["+",
                   "-",
                   "*",
                   "%",
                   "/",
                   "~",
                   "!",
                   ">",
                   "<",
                   "=",
                   "&",
                   "^",
                   "|"])
                  `shouldBe`
                  [OpTok PlusSign dummyLexDat,
                   OpTok MinusSign dummyLexDat,
                   OpTok Asterisk dummyLexDat,
                   OpTok Percent dummyLexDat,
                   OpTok Backslash dummyLexDat,
                   OpTok Tilde dummyLexDat,
                   OpTok Bang dummyLexDat,
                   OpTok RightArrow dummyLexDat,
                   OpTok LeftArrow dummyLexDat,
                   OpTok EqualSign dummyLexDat,
                   OpTok Ampersand dummyLexDat,
                   OpTok Caret dummyLexDat,
                   OpTok Pipe dummyLexDat]

                it "Should correctly lex all two-character operators" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["||",
                   "&&",
                   ">=",
                   "<=",
                   "==",
                   "!=",
                   "+=",
                   "-=",
                   "*=",
                   "/=",
                   "%=",
                   "++",
                   "--",
                   "&=",
                   "^=",
                   "|=",
                   "<<",
                   ">>"])
                  `shouldBe`
                  [OpTok PipePipe dummyLexDat,
                   OpTok AmpAmp dummyLexDat,
                   OpTok RightArrowEqual dummyLexDat,
                   OpTok LeftArrowEqual dummyLexDat,
                   OpTok EqualEqual dummyLexDat,
                   OpTok BangEqual dummyLexDat,
                   OpTok PlusEqual dummyLexDat,
                   OpTok MinusEqual dummyLexDat,
                   OpTok AsteriskEqual dummyLexDat,
                   OpTok BackslashEqual dummyLexDat,
                   OpTok PercentEqual dummyLexDat,
                   OpTok PlusPlus dummyLexDat,
                   OpTok MinusMinus dummyLexDat,
                   OpTok AmpEqual dummyLexDat,
                   OpTok CaretEqual dummyLexDat,
                   OpTok PipeEqual dummyLexDat,
                   OpTok DoubleLeftArrow dummyLexDat,
                   OpTok DoubleRightArrow dummyLexDat]

                it "Should correctly lex all three-character opertators" $
                  (dummyData $
                  concatMap (fromRight [] . tokenize) ["<<=",">>="])
                  `shouldBe` [OpTok DoubleLArrowEqual dummyLexDat,OpTok DoubleRArrowEqual dummyLexDat]

                it "Should correctly lex a mix of different length operators" $
                  (dummyData $
                  concatMap
                  (fromRight [] . tokenize)
                  ["+",
                   "||",
                   "<<=",
                   "*"])
                  `shouldBe`
                  [OpTok PlusSign dummyLexDat,
                   OpTok PipePipe dummyLexDat,
                   OpTok DoubleLArrowEqual dummyLexDat,
                   OpTok Asterisk dummyLexDat]

                it "Should correctly lex easily confused operators" $
                  (dummyData $
                  concatMap (fromRight [] . tokenize) ["++","+","+="])
                  `shouldBe`
                  [OpTok PlusPlus dummyLexDat,OpTok PlusSign dummyLexDat,OpTok PlusEqual dummyLexDat]

                it "Should correctly lex valid numbers" $
                  (dummyData $
                  concatMap (fromRight [] . tokenize) ["123","1","0"])
                  `shouldBe`
                  [ConstInt 123 dummyLexDat,ConstInt 1 dummyLexDat,ConstInt 0 dummyLexDat]

                it "Should correctly lex a variable declaration" $
                  (dummyData $
                  fromRight [] (tokenize "int a;"))
                  `shouldBe` [Keyword Int dummyLexDat,Ident "a" dummyLexDat,Separator SemiColon dummyLexDat]

                it "Should correctly lex a simple function" $
                  (dummyData $
                  fromRight [] (tokenize "int main() { return 2; }"))
                  `shouldBe` [Keyword Int dummyLexDat,
                              Ident "main" dummyLexDat,
                              OpenBracket OpenParen dummyLexDat,
                              CloseBracket CloseParen dummyLexDat,
                              OpenBracket OpenBrace dummyLexDat,
                              Keyword Return dummyLexDat,
                              ConstInt 2 dummyLexDat,
                              Separator SemiColon dummyLexDat,
                              CloseBracket CloseBrace dummyLexDat]
