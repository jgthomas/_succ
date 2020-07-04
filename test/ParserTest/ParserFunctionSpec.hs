
module ParserTest.ParserFunctionSpec (parserFunctionTest) where


import Test.Hspec

import ParserTest.TestUtility (extractFunctionTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Tokens
import Types.Type


parserFunctionTest :: IO ()
parserFunctionTest = hspec $ do
        describe "Build abstract syntax trees for functions" $ do

                it "Should build a function declaration tree" $
                  (extractFunctionTree [Keyword Int,
                                        Ident "dog",
                                        OpenBracket OpenParen,
                                        CloseBracket CloseParen,
                                        SemiColon
                                       ])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "dog"
                               []
                               Nothing
                               mockNodeDat
                              ]

                it "Should build a simple function definition tree" $
                  (extractFunctionTree [Keyword Int,
                                        Ident "main",
                                        OpenBracket OpenParen,
                                        CloseBracket CloseParen,
                                        OpenBracket OpenBrace,
                                        CloseBracket CloseBrace
                                       ])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [])
                               mockNodeDat]

                it "Should build a function definition with arguments tree" $
                  (extractFunctionTree [Keyword Int,
                                        Ident "main",
                                        OpenBracket OpenParen,
                                        CloseBracket CloseParen,
                                        OpenBracket OpenBrace,
                                        CloseBracket CloseBrace
                                       ])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [])
                               mockNodeDat]
