
module ParserTest.ParserDeclarationSpec (parserDeclarationTest) where


import Test.Hspec

import ParserTest.TestUtility (extractDeclarationTree)
import TestUtility            (mockNodeDat)
import Types.AST
import Types.Operator
import Types.Tokens
import Types.Type


parserDeclarationTest :: IO ()
parserDeclarationTest = hspec $ do
        describe "Build abstract syntax trees for declarations" $ do

                it "Should build a tree for variable declaration" $
                  (extractDeclarationTree [Keyword Int, Ident "a"])
                  `shouldBe`
                  ProgramNode [DeclarationNode
                               (VarNode "a" mockNodeDat)
                               IntVar
                               Nothing
                               mockNodeDat]

                it "Should build a tree for variable declaration and assignment" $
                  (extractDeclarationTree [Keyword Int,
                                           Ident "a",
                                           OpTok EqualSign,
                                           ConstInt 2])
                  `shouldBe`
                  ProgramNode [DeclarationNode
                               (VarNode "a" mockNodeDat)
                               IntVar
                               (Just (AssignmentNode
                                (VarNode "a" mockNodeDat)
                                (ConstantNode 2 mockNodeDat)
                                Assignment
                                mockNodeDat))
                               mockNodeDat]

                it "Should build a tree for a pointer declaration" $
                  (extractDeclarationTree [Keyword Int,
                                           OpTok Asterisk,
                                           Ident "a"])
                  `shouldBe`
                  ProgramNode [PointerNode
                               (VarNode "a" mockNodeDat)
                               IntPointer
                               Nothing
                               mockNodeDat]

                it "Should build a tree for a pointer declaration and assignment" $
                  (extractDeclarationTree [Keyword Int,
                                           OpTok Asterisk,
                                           Ident "a",
                                           OpTok EqualSign,
                                           OpTok Ampersand,
                                           Ident "b"])
                  `shouldBe`
                  ProgramNode [PointerNode
                               (VarNode "a" mockNodeDat)
                               IntPointer
                               (Just (AssignmentNode
                                (VarNode "a" mockNodeDat)
                                (AddressOfNode "b" mockNodeDat)
                                Assignment
                                mockNodeDat)
                               )
                               mockNodeDat]

                it "Should build a tree for an array declaration" $
                  (extractDeclarationTree [Keyword Int,
                                           Ident "a",
                                           OpenBracket OpenSqBracket,
                                           ConstInt 2,
                                           CloseBracket CloseSqBracket])
                  `shouldBe`
                  ProgramNode [ArrayNode
                               (ArrayDeclareNode 2
                                (VarNode "a" mockNodeDat)
                                IntArray
                                Nothing
                                mockNodeDat)]

                it "Should build a tree for an array declaration and assignment" $
                  (extractDeclarationTree [Keyword Int,
                                           Ident "a",
                                           OpenBracket OpenSqBracket,
                                           ConstInt 2,
                                           CloseBracket CloseSqBracket,
                                           OpTok EqualSign,
                                           OpenBracket OpenBrace,
                                           ConstInt 10,
                                           Comma,
                                           ConstInt 20,
                                           CloseBracket CloseBrace,
                                           SemiColon
                                          ]
                  )
                  `shouldBe`
                  ProgramNode [ArrayNode
                               (ArrayDeclareNode
                                2
                                (VarNode "a" mockNodeDat)
                                IntArray
                                (Just $ ArrayNode
                                 (ArrayItemsNode
                                  (VarNode "a" mockNodeDat)
                                  [ArrayNode
                                   (ArraySingleItemNode
                                    (ConstantNode 10 mockNodeDat)
                                    mockNodeDat),
                                   ArrayNode
                                   (ArraySingleItemNode
                                    (ConstantNode 20 mockNodeDat)
                                    mockNodeDat
                                   )
                                  ]
                                  mockNodeDat
                                 )
                                )
                                mockNodeDat
                               )
                              ]

