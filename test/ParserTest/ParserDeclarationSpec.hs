
module ParserTest.ParserDeclarationSpec (parserDeclarationTest) where


import Test.Hspec

import ParserTest.TestUtility (extractDeclarationTree)
import TestUtility            (makeNodeDat)
import Types.AST
import Types.Tokens
import Types.Type


parserDeclarationTest :: IO ()
parserDeclarationTest = hspec $ do
        describe "Build abstract syntax trees for declarations" $ do

                it "Should build a variable declaration tree" $
                  (extractDeclarationTree [Keyword Int, Ident "a"])
                  `shouldBe`
                  ProgramNode [DeclarationNode
                               (VarNode "a" makeNodeDat)
                               IntVar
                               Nothing
                               makeNodeDat]

