
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import ParserTest.TestUtility (extractExpressionError, extractExpressionTree)
import TestUtility            (makeNodeDat)
import Types.AST
import Types.Error
import Types.Operator
import Types.Tokens


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Build abstract syntax trees for expressions" $ do

                it "Should build a variable tree" $
                  (extractExpressionTree [Ident "a"])
                  `shouldBe`
                  ProgramNode [VarNode "a" makeNodeDat]

                it "Should build basic assignment tree" $
                  (extractExpressionTree [Ident "a", OpTok EqualSign, ConstInt 2])
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" makeNodeDat)
                               (ConstantNode 2 makeNodeDat)
                               Assignment
                               makeNodeDat]

                it "Should build unary operator tree" $
                  (extractExpressionTree [OpTok MinusSign, Ident "a"])
                  `shouldBe`
                  ProgramNode [UnaryNode
                               (VarNode "a" makeNodeDat)
                               (Unary Negate)
                               makeNodeDat]

                it "Should build binary operator tree" $
                  (extractExpressionTree [Ident "a", OpTok PlusSign, ConstInt 2])
                  `shouldBe`
                  ProgramNode [BinaryNode
                               (VarNode "a" makeNodeDat)
                               (ConstantNode 2 makeNodeDat)
                               Plus
                               makeNodeDat]

        describe "Throw errors on bad input" $ do

                it "Should throw error on empty input" $
                  (extractExpressionError [])
                  `shouldBe`
                  ParserError (LexDataError [])

