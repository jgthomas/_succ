
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import Parser.TokConvert
import ParserTest.TestUtility (extractExpressionError, extractExpressionTree)
import TestUtility            (mockNodeDat)
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
                  ProgramNode [ConstantNode 3 mockNodeDat]

                it "Should build a variable tree" $
                  (extractExpressionTree [Ident "a"])
                  `shouldBe`
                  ProgramNode [VarNode "a" mockNodeDat]

                it "Should build basic assignment tree" $
                  (extractExpressionTree [Ident "a", OpTok EqualSign, ConstInt 2])
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" mockNodeDat)
                               (ConstantNode 2 mockNodeDat)
                               Assignment
                               mockNodeDat]

                it "Should build a null expression tree" $
                  (extractExpressionTree [SemiColon])
                  `shouldBe`
                  ProgramNode [NullExprNode mockNodeDat]

                it "Should build unary operator trees" $
                  (map (extractExpressionTree . makeUnaryTokenList) unaryOpTokens)
                  `shouldBe`
                  (map makeUnaryTree unaryOpTokens)

                it "Should build unary post operator trees" $
                  (map (extractExpressionTree . makeUnaryPostOpTokenList) unaryPostOpTokens)
                  `shouldBe`
                  (map makeUnaryPostOpTree unaryPostOpTokens)

                it "Should build binary operator trees" $
                  (map (extractExpressionTree . makeBinaryTokenList) binaryOpTokens)
                  `shouldBe`
                  (map makeBinaryTree binaryOpTokens)

                it "Should build binary shadow assignment trees" $
                  (map (extractExpressionTree . makeBinaryTokenList) binaryShadowAssignTokens)
                  `shouldBe`
                  (map makeBinaryShadowAssignTree binaryShadowAssignTokens)

                it "Should build a ternary operator tree" $
                  (extractExpressionTree [ConstInt 2,
                                          OpTok EqualEqual,
                                          ConstInt 2,
                                          QuestMark,
                                          ConstInt 10,
                                          Colon,
                                          ConstInt 6,
                                          SemiColon
                                         ]
                  )
                  `shouldBe`
                  ProgramNode [TernaryNode
                               (BinaryNode
                                (ConstantNode 2 mockNodeDat)
                                (ConstantNode 2 mockNodeDat)
                                Equal
                                mockNodeDat
                               )
                               (ConstantNode 10 mockNodeDat)
                               (ConstantNode 6 mockNodeDat)
                               mockNodeDat
                              ]

                it "Should build a simple function call tree" $
                  (extractExpressionTree [Ident "dog", OpenBracket OpenParen, CloseBracket CloseParen])
                  `shouldBe`
                  ProgramNode [FuncCallNode "dog" [] mockNodeDat]

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
                               [ArgNode (ConstantNode 3 mockNodeDat) mockNodeDat,
                                ArgNode (ConstantNode 4 mockNodeDat) mockNodeDat
                               ]
                               mockNodeDat]

                it "Should build a pointer dereference tree" $
                  (extractExpressionTree [OpTok Asterisk, Ident "b"])
                  `shouldBe`
                  ProgramNode [DereferenceNode "b" mockNodeDat]

                it "Should build a tree with a parenthesised calculation" $
                  (extractExpressionTree [Ident "a",
                                          OpTok EqualSign,
                                          OpenBracket OpenParen,
                                          ConstInt 2,
                                          OpTok MinusSign,
                                          ConstInt 1,
                                          CloseBracket CloseParen,
                                          OpTok Asterisk,
                                          ConstInt 3,
                                          SemiColon
                                         ]
                  )
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" mockNodeDat)
                               (BinaryNode
                                (BinaryNode
                                 (ConstantNode 2 mockNodeDat)
                                 (ConstantNode 1 mockNodeDat)
                                 Minus
                                 mockNodeDat
                                )
                                (ConstantNode 3 mockNodeDat)
                                Multiply
                                mockNodeDat
                               )
                               Assignment
                               mockNodeDat
                              ]

        describe "Throw errors on bad input" $ do

                it "Should throw error on empty input" $
                  (extractExpressionError [])
                  `shouldBe`
                  ParserError (LexDataError [])


unaryPostOpTokens :: [OpTok]
unaryPostOpTokens = [
                    PlusPlus,
                    MinusMinus
                    ]


makeUnaryPostOpTree :: OpTok -> Tree
makeUnaryPostOpTree opTok =
        ProgramNode [UnaryNode
                     (VarNode "a" mockNodeDat)
                     (tokToPostUnaryOp opTok)
                     mockNodeDat]


makeUnaryPostOpTokenList :: OpTok -> [Token]
makeUnaryPostOpTokenList tok = [Ident "a", OpTok tok]


unaryOpTokens :: [OpTok]
unaryOpTokens = [
                MinusSign,
                PlusSign,
                Bang,
                Tilde,
                PlusPlus,
                MinusMinus
                ]


makeUnaryTokenList :: OpTok -> [Token]
makeUnaryTokenList tok = [OpTok tok, Ident "a"]


makeUnaryTree :: OpTok -> Tree
makeUnaryTree opTok =
        ProgramNode [UnaryNode
                     (VarNode "a" mockNodeDat)
                     (tokToUnaryOp opTok)
                     mockNodeDat]


makeBinaryTokenList :: OpTok -> [Token]
makeBinaryTokenList tok = [Ident "a", OpTok tok, ConstInt 2]


makeBinaryTree :: OpTok -> Tree
makeBinaryTree opTok =
        ProgramNode [BinaryNode
                     (VarNode "a" mockNodeDat)
                     (ConstantNode 2 mockNodeDat)
                     (tokToBinOp opTok)
                     mockNodeDat]


makeBinaryShadowAssignTree :: OpTok -> Tree
makeBinaryShadowAssignTree opTok =
        ProgramNode [AssignmentNode
                     (VarNode "a" mockNodeDat)
                     (ConstantNode 2 mockNodeDat)
                     (BinaryOp $ tokToBinOp opTok)
                     mockNodeDat]


binaryOpTokens :: [OpTok]
binaryOpTokens = [
                 PlusSign,
                 MinusSign,
                 Backslash,
                 Asterisk,
                 Percent,
                 EqualEqual,
                 BangEqual,
                 RightArrow,
                 LeftArrow,
                 RightArrowEqual,
                 LeftArrowEqual,
                 PipePipe,
                 AmpAmp,
                 Caret,
                 Ampersand,
                 Pipe,
                 DoubleLeftArrow,
                 DoubleRightArrow
                 ]

binaryShadowAssignTokens :: [OpTok]
binaryShadowAssignTokens = [
                           PlusEqual,
                           MinusEqual,
                           AsteriskEqual,
                           BackslashEqual,
                           PercentEqual,
                           CaretEqual,
                           AmpEqual,
                           PipeEqual,
                           DoubleLArrowEqual,
                           DoubleRArrowEqual
                           ]
