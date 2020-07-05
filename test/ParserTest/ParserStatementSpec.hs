
module ParserTest.ParserStatementSpec (parserStatementTest) where


import Test.Hspec

import ParserTest.TestUtility (extractStatementTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Operator
import Types.Tokens


parserStatementTest :: IO ()
parserStatementTest = hspec $ do
        describe "Build abstract syntax trees for statements" $ do

                it "Should build a tree for a continue statement" $
                  (extractStatementTree [Keyword Continue, SemiColon])
                  `shouldBe`
                  ProgramNode [ContinueNode mockNodeDat]

                it "Should build a tree for a break statement" $
                  (extractStatementTree [Keyword Break, SemiColon])
                  `shouldBe`
                  ProgramNode [BreakNode mockNodeDat]

                it "Should build a tree for a simple compound statement" $
                  (extractStatementTree [OpenBracket OpenBrace,
                                         ConstInt 2,
                                         OpTok PlusSign,
                                         ConstInt 2,
                                         SemiColon,
                                         Keyword Break,
                                         SemiColon,
                                         CloseBracket CloseBrace
                                        ])
                  `shouldBe`
                  ProgramNode [CompoundStmtNode
                               [
                                ExprStmtNode
                                (BinaryNode
                                 (ConstantNode 2 mockNodeDat)
                                 (ConstantNode 2 mockNodeDat)
                                 Plus
                                 mockNodeDat
                                )
                                mockNodeDat,
                                BreakNode mockNodeDat
                               ]
                               mockNodeDat
                              ]
