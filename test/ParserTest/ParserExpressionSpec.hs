
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

                it "Should build a constant tree" $
                  (extractExpressionTree [ConstInt 3])
                  `shouldBe`
                  ProgramNode [ConstantNode 3 makeNodeDat]

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

                it "Should build a simple function call tree" $
                  (extractExpressionTree [Ident "dog", OpenBracket OpenParen, CloseBracket CloseParen])
                  `shouldBe`
                  ProgramNode [FuncCallNode "dog" [] makeNodeDat]

                it "Should build a function call node with arguments tree" $
                  (extractExpressionTree [Ident "cat",
                                          OpenBracket OpenParen,
                                          ConstInt 3,
                                          Comma,
                                          ConstInt 4,
                                          CloseBracket CloseParen
                                         ])
                  `shouldBe`
                  ProgramNode [FuncCallNode "cat"
                               [ArgNode (ConstantNode 3 makeNodeDat) makeNodeDat,
                                ArgNode (ConstantNode 4 makeNodeDat) makeNodeDat
                               ]
                               makeNodeDat]

        describe "Throw errors on bad input" $ do

                it "Should throw error on empty input" $
                  (extractExpressionError [])
                  `shouldBe`
                  ParserError (LexDataError [])

