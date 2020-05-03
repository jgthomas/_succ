
module LexerSpec (lexerTest) where


import Data.Either
import Test.Hspec

import Error.Error
import Lexer.Lexer
import Lexer.LexTab
import Types.Tokens


lexerTest :: IO ()
lexerTest = hspec $ do
        describe "Lex input string into tokens" $ do
                it "Should correctly lex all separator characters" $
                  (map tok $
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
                  [OpenBracket OpenParen,
                   CloseBracket CloseParen,
                   OpenBracket OpenBrace,
                   CloseBracket CloseBrace,
                   SemiColon,
                   Colon,
                   QuestMark,
                   Comma,
                   OpenBracket OpenSqBracket,
                   CloseBracket CloseSqBracket]

                it "Should correctly lex all language keywords" $
                  (map tok $
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
                  [Keyword Int,
                   Keyword Return,
                   Keyword If,
                   Keyword Else,
                   Keyword For,
                   Keyword While,
                   Keyword Do,
                   Keyword Break,
                   Keyword Continue]

                it "Should correctly lex valid identifiers" $
                  (map tok $
                  concatMap
                  (fromRight [] . tokenize)
                  ["main",
                   "dog",
                   "_cat",
                   "Mouse"])
                  `shouldBe`
                  [Ident "main",
                   Ident "dog",
                   Ident "_cat",
                   Ident "Mouse"]

                it "Should correctly lex all single-character operators" $
                  (map tok $
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
                  [OpTok PlusSign,
                   OpTok MinusSign,
                   OpTok Asterisk,
                   OpTok Percent,
                   OpTok Backslash,
                   OpTok Tilde,
                   OpTok Bang,
                   OpTok RightArrow,
                   OpTok LeftArrow,
                   OpTok EqualSign,
                   OpTok Ampersand,
                   OpTok Caret,
                   OpTok Pipe]

                it "Should correctly lex all two-character operators" $
                  (map tok $
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
                  [OpTok PipePipe,
                   OpTok AmpAmp,
                   OpTok RightArrowEqual,
                   OpTok LeftArrowEqual,
                   OpTok EqualEqual,
                   OpTok BangEqual,
                   OpTok PlusEqual,
                   OpTok MinusEqual,
                   OpTok AsteriskEqual,
                   OpTok BackslashEqual,
                   OpTok PercentEqual,
                   OpTok PlusPlus,
                   OpTok MinusMinus,
                   OpTok AmpEqual,
                   OpTok CaretEqual,
                   OpTok PipeEqual,
                   OpTok DoubleLeftArrow,
                   OpTok DoubleRightArrow]

                it "Should correctly lex all three-character opertators" $
                  (map tok $
                  concatMap (fromRight [] . tokenize) ["<<=",">>="])
                  `shouldBe` [OpTok DoubleLArrowEqual,OpTok DoubleRArrowEqual]

                it "Should correctly lex a mix of different length operators" $
                  (map tok $
                  concatMap
                  (fromRight [] . tokenize)
                  ["+",
                   "||",
                   "<<=",
                   "*"])
                  `shouldBe`
                  [OpTok PlusSign,
                   OpTok PipePipe,
                   OpTok DoubleLArrowEqual,
                   OpTok Asterisk]

                it "Should correctly lex easily confused operators" $
                  (map tok $
                  concatMap (fromRight [] . tokenize) ["++","+","+="])
                  `shouldBe` [OpTok PlusPlus,OpTok PlusSign,OpTok PlusEqual]

                it "Should correctly lex valid numbers" $
                  (map tok $
                  concatMap (fromRight [] . tokenize) ["123","1","0"])
                  `shouldBe` [ConstInt 123,ConstInt 1,ConstInt 0]

                it "Should correctly lex a variable declaration" $
                  (map tok $
                  fromRight [] (tokenize "int a;"))
                  `shouldBe` [Keyword Int,Ident "a",SemiColon]

                it "Should correctly lex a simple function" $
                  (map tok $
                  fromRight [] (tokenize "int main() { return 2; }"))
                  `shouldBe` [Keyword Int,
                              Ident "main",
                              OpenBracket OpenParen,
                              CloseBracket CloseParen,
                              OpenBracket OpenBrace,
                              Keyword Return,
                              ConstInt 2,
                              SemiColon,
                              CloseBracket CloseBrace]

        describe "Throw errors on bad input" $ do
                it "Should throw error for unrecognised character" $
                  fromLeft ImpossibleError (tokenize "$")
                  `shouldBe` LexerError (UnexpectedInput "$")

                it "Should throw error for empty input" $
                  fromLeft ImpossibleError (tokenize "")
                  `shouldBe` LexerError EmptyInput
