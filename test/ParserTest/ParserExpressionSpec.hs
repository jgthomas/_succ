
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import ParserTest.TestUtility
import TestUtility            (makeNodeDat)
import Types.AST
import Types.Tokens


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Dummy test" $ do
                it "Should print dummy data" $
                  (getParsed . extractParsed . makeLexData $ [Ident "a", SemiColon])
                  `shouldBe`
                  ProgramNode [VarNode "a" makeNodeDat]

