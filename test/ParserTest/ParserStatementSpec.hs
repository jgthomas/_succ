
module ParserTest.ParserStatementSpec (parserStatementTest) where


import Test.Hspec

import ParserTest.TestUtility (extractStatementTree)
import TestUtility            (mockNodeDat)
import Types.AST
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
