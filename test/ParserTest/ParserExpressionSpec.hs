
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
                  (extractExpressionTree [ConstInt 3 dummyLexDat])
                  `shouldBe`
                  ProgramNode [ConstantNode 3 mockNodeDat]

                it "Should build a variable tree" $
                  (extractExpressionTree [Ident "a" dummyLexDat])
                  `shouldBe`
                  ProgramNode [VarNode "a" mockNodeDat]

                it "Should build basic assignment tree" $
                  (extractExpressionTree [Ident "a" dummyLexDat,
                                          OpTok EqualSign dummyLexDat,
                                          ConstInt 2 dummyLexDat])
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" mockNodeDat)
                               (ConstantNode 2 mockNodeDat)
                               Assignment
                               mockNodeDat]

                it "Should build a null expression tree" $
                  (extractExpressionTree [Separator SemiColon dummyLexDat])
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
                  (extractExpressionTree [ConstInt 2 dummyLexDat,
                                          OpTok EqualEqual dummyLexDat,
                                          ConstInt 2 dummyLexDat,
                                          Separator QuestMark dummyLexDat,
                                          ConstInt 10 dummyLexDat,
                                          Separator Colon dummyLexDat,
                                          ConstInt 6 dummyLexDat,
                                          Separator SemiColon dummyLexDat
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
                  (extractExpressionTree [Ident "dog" dummyLexDat,
                                          OpenBracket OpenParen dummyLexDat,
                                          CloseBracket CloseParen dummyLexDat])
                  `shouldBe`
                  ProgramNode [FuncCallNode "dog" [] mockNodeDat]

                it "Should build a function call node with arguments tree" $
                  (extractExpressionTree [Ident "cat" dummyLexDat,
                                          OpenBracket OpenParen dummyLexDat,
                                          ConstInt 3 dummyLexDat,
                                          Separator Comma dummyLexDat,
                                          ConstInt 4 dummyLexDat,
                                          CloseBracket CloseParen dummyLexDat
                                         ])
                  `shouldBe`
                  ProgramNode [FuncCallNode "cat"
                               [ArgNode (ConstantNode 3 mockNodeDat) mockNodeDat,
                                ArgNode (ConstantNode 4 mockNodeDat) mockNodeDat
                               ]
                               mockNodeDat]

                it "Should build a pointer dereference tree" $
                  (extractExpressionTree [OpTok Asterisk dummyLexDat, Ident "b" dummyLexDat])
                  `shouldBe`
                  ProgramNode [DereferenceNode "b" mockNodeDat]

                it "Should build a tree with a parenthesised calculation" $
                  (extractExpressionTree [Ident "a" dummyLexDat,
                                          OpTok EqualSign dummyLexDat,
                                          OpenBracket OpenParen dummyLexDat,
                                          ConstInt 2 dummyLexDat,
                                          OpTok MinusSign dummyLexDat,
                                          ConstInt 1 dummyLexDat,
                                          CloseBracket CloseParen dummyLexDat,
                                          OpTok Asterisk dummyLexDat,
                                          ConstInt 3 dummyLexDat,
                                          Separator SemiColon dummyLexDat
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

                it "Should build a tree assigning item from an array" $
                  (extractExpressionTree [Ident "a" dummyLexDat,
                                          OpTok EqualSign dummyLexDat,
                                          Ident "arr" dummyLexDat,
                                          OpenBracket OpenSqBracket dummyLexDat,
                                          ConstInt 1 dummyLexDat,
                                          CloseBracket CloseSqBracket dummyLexDat,
                                          Separator SemiColon dummyLexDat
                                         ]
                  )
                  `shouldBe`
                  ProgramNode [AssignmentNode
                               (VarNode "a" mockNodeDat)
                               (ArrayNode
                                (ArrayItemAccess
                                 1
                                 (VarNode "arr" mockNodeDat)
                                 mockNodeDat
                                )
                               )
                               Assignment
                               mockNodeDat
                              ]

                it "Should build a tree assigning an item to an array index" $
                  (extractExpressionTree [Ident "arr" dummyLexDat,
                                          OpenBracket OpenSqBracket dummyLexDat,
                                          ConstInt 1 dummyLexDat,
                                          CloseBracket CloseSqBracket dummyLexDat,
                                          OpTok EqualSign dummyLexDat,
                                          ConstInt 10 dummyLexDat,
                                          Separator SemiColon dummyLexDat
                                         ]
                  )
                  `shouldBe`
                  ProgramNode [ArrayNode
                               (ArrayAssignPosNode
                                (ArrayNode
                                 (ArrayItemAssign
                                  1
                                  (VarNode "arr" mockNodeDat)
                                  mockNodeDat
                                 )
                                )
                                (ConstantNode 10 mockNodeDat)
                                Assignment
                                mockNodeDat
                               )
                              ]

                it "Should build a tree for assigning from a dereferenced node" $
                  (extractExpressionTree [OpTok Asterisk dummyLexDat,
                                          Ident "b" dummyLexDat,
                                          OpTok PlusEqual dummyLexDat,
                                          ConstInt 10 dummyLexDat,
                                          Separator SemiColon dummyLexDat
                                         ]
                  )
                  `shouldBe`
                  ProgramNode [AssignDereferenceNode
                               (DereferenceNode "b" mockNodeDat)
                               (ConstantNode 10 mockNodeDat)
                               (BinaryOp Plus)
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
makeUnaryPostOpTokenList tok = [Ident "a" dummyLexDat, OpTok tok dummyLexDat]


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
makeUnaryTokenList tok = [OpTok tok dummyLexDat, Ident "a" dummyLexDat]


makeUnaryTree :: OpTok -> Tree
makeUnaryTree opTok =
        ProgramNode [UnaryNode
                     (VarNode "a" mockNodeDat)
                     (tokToUnaryOp opTok)
                     mockNodeDat]


makeBinaryTokenList :: OpTok -> [Token]
makeBinaryTokenList tok = [Ident "a" dummyLexDat, OpTok tok dummyLexDat, ConstInt 2 dummyLexDat]


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
