
module LexerSpec (lexerTest) where


import Data.Either
import Test.Hspec

import Error
import Lexer
import Tokens


lexerTest :: IO ()
lexerTest = hspec $ do
        describe "Lex input string into tokens" $ do
                it "Should correctly lex all separator characters" $
                  concatMap
                  (fromRight [] . tokenize)
                  ["(",
                   ")",
                   "{",
                   "}",
                   ";",
                   ":",
                   "?",
                   ","]
                  `shouldBe`
                  [OpenParen,
                   CloseParen,
                   OpenBrace,
                   CloseBrace,
                   SemiColon,
                   Colon,
                   QuestMark,
                   Comma]

                it "Should correctly lex all language keywords" $
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
                   "continue"]
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
                  concatMap
                  (fromRight [] . tokenize)
                  ["main",
                   "dog",
                   "_cat",
                   "Mouse"]
                  `shouldBe`
                  [Ident "main",
                   Ident "dog",
                   Ident "_cat",
                   Ident "Mouse"]

                it "Should correctly lex all single-character operators" $
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
                   "|"]
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
                   ">>"]
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
                  concatMap (fromRight [] . tokenize) ["<<=",">>="]
                  `shouldBe` [OpTok DoubleLArrowEqual,OpTok DoubleRArrowEqual]

                it "Should correctly lex a mix of different length operators" $
                  concatMap
                  (fromRight [] . tokenize)
                  ["+",
                   "||",
                   "<<=",
                   "*"]
                  `shouldBe`
                  [OpTok PlusSign,
                   OpTok PipePipe,
                   OpTok DoubleLArrowEqual,
                   OpTok Asterisk]

                it "Should correctly lex easily confused operators" $
                  concatMap (fromRight [] . tokenize) ["++","+","+="]
                  `shouldBe` [OpTok PlusPlus,OpTok PlusSign,OpTok PlusEqual]

                it "Should correctly lex valid numbers" $
                  concatMap (fromRight [] . tokenize) ["123","1","0"]
                  `shouldBe` [ConstInt 123,ConstInt 1,ConstInt 0]

                it "Should correctly lex a variable declaration" $
                  fromRight [] (tokenize "int a;")
                  `shouldBe` [Keyword Int,Ident "a",SemiColon]

        describe "Throw errors on bad input" $ do
                it "Should throw error for unrecognised character" $
                  fromLeft ImpossibleError (tokenize "$")
                  `shouldBe` LexerError (BadInput "$")

                it "Should throw error for empty input" $
                  fromLeft ImpossibleError (tokenize "")
                  `shouldBe` LexerError EmptyInput
