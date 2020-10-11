
module LexerTest.LexerMetadataTestSpec (lexerMetadataTest) where


import Data.Either
import Test.Hspec

import Lexer.Lexer
import LexerTest.TestUtility (metaData)
import Types.Tokens


lexerMetadataTest :: IO ()
lexerMetadataTest = hspec $ do
        describe "Lexing input captures correct metadata" $ do

                it "Should build metadata for syntactic tokens" $
                  (map metaData $ fromRight []
                   (tokenize "(\n)\n{\n}\n;\n:\n?\n,\n[\n]"))
                  `shouldBe`
                  [("(", 1),
                   (")", 2),
                   ("{", 3),
                   ("}", 4),
                   (";", 5),
                   (":", 6),
                   ("?", 7),
                   (",", 8),
                   ("[", 9),
                   ("]", 10)
                  ]

                it "Should build metadata for language keywords" $
                  (map metaData $ fromRight []
                   (tokenize "int\nreturn\nif\nelse\nfor\nwhile\ndo\nbreak\ncontinue"))
                  `shouldBe`
                  [("int", 1),
                   ("return", 2),
                   ("if", 3),
                   ("else", 4),
                   ("for", 5),
                   ("while", 6),
                   ("do", 7),
                   ("break", 8),
                   ("continue", 9)
                  ]

                it "Should build metadata for valid identifiers" $
                  (map metaData $ fromRight []
                   (tokenize "main\ndog\n_cat\nMouse"))
                  `shouldBe`
                  [("main", 1),
                   ("dog", 2),
                   ("_cat", 3),
                   ("Mouse", 4)
                  ]

                it "Should build metadata for single-character operators" $
                  (map metaData $ fromRight []
                   (tokenize "+\n-\n*\n%\n/\n~\n!\n>\n<\n=\n&\n^\n|\n"))
                  `shouldBe`
                  [("+", 1),
                   ("-", 2),
                   ("*", 3),
                   ("%", 4),
                   ("/", 5),
                   ("~", 6),
                   ("!", 7),
                   (">", 8),
                   ("<", 9),
                   ("=", 10),
                   ("&", 11),
                   ("^", 12),
                   ("|", 13)
                  ]

                it "Should build metadata for two-character operators" $
                  (map metaData $ fromRight []
                   (tokenize
                    "||\n&&\n>=\n<=\n==\n!=\n+=\n-=\n*=\n/=\n%=\n++\n--\n&=\n^=\n|=\n<<\n>>\n"))
                  `shouldBe`
                  [("||", 1),
                   ("&&", 2),
                   (">=", 3),
                   ("<=", 4),
                   ("==", 5),
                   ("!=", 6),
                   ("+=", 7),
                   ("-=", 8),
                   ("*=", 9),
                   ("/=", 10),
                   ("%=", 11),
                   ("++", 12),
                   ("--", 13),
                   ("&=", 14),
                   ("^=", 15),
                   ("|=", 16),
                   ("<<", 17),
                   (">>", 18)
                  ]

                it "Should build metadata for three-character operators" $
                  (map metaData $ fromRight []
                   (tokenize "<<=\n>>="))
                  `shouldBe`
                  [("<<=", 1),
                   (">>=", 2)
                  ]

                it "Should record the correct line for each token" $
                  (map (line . tokenData) $
                  fromRight [] (tokenize "int main() { \n return 2;\n}"))
                  `shouldBe` [1,1,1,1,1,2,2,2,3]
