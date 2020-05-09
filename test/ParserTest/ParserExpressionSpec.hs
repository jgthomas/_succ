
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import ParserTest.TestUtility (extractExpressionTree)
import TestUtility            (makeNodeDat)
import Types.AST
import Types.Operator
import Types.Tokens


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Build abstract syntax trees for expressions" $ do

                it "Should build a variable syntax tree" $
                  (extractExpressionTree [Ident "a", SemiColon])
                  `shouldBe`
                  ProgramNode [VarNode "a" makeNodeDat]

                it "Should build unary node tree" $
                  (extractExpressionTree [OpTok MinusSign, Ident "a", SemiColon])
                  `shouldBe`
                  ProgramNode [UnaryNode
                               (VarNode "a" makeNodeDat)
                               (Unary Negate)
                               makeNodeDat]

                it "Should build basic assignment tree" $
                  (extractExpressionTree [Ident "a", OpTok EqualSign, ConstInt 2, SemiColon])
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" makeNodeDat)
                               (ConstantNode 2 makeNodeDat)
                               Assignment
                               makeNodeDat]

