module ParserTest.ParserDeclarationSpec
  ( parserDeclarationTest,
  )
where

import ParserTest.TestUtility (extractDeclarationError, extractDeclarationTree)
import Test.Hspec
import TestUtility (mockNodeDat)
import Types.AST
import Types.Error
import Types.Operator
import Types.Tokens
import Types.Type

parserDeclarationTest :: IO ()
parserDeclarationTest = hspec $ do
  describe "Build abstract syntax trees for declarations" $ do
    it "Should build a tree for variable declaration" $
      (extractDeclarationTree [Keyword Int dummyLexDat, Ident "a" dummyLexDat])
        `shouldBe` ProgramNode
          [ DeclarationNode
              (VarNode "a" mockNodeDat)
              IntVar
              Nothing
              mockNodeDat
          ]
    it "Should build a tree for variable declaration and assignment" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            OpTok EqualSign dummyLexDat,
            ConstInt 2 dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ DeclarationNode
              (VarNode "a" mockNodeDat)
              IntVar
              ( Just
                  ( AssignmentNode
                      (VarNode "a" mockNodeDat)
                      (ConstantNode 2 mockNodeDat)
                      Assignment
                      mockNodeDat
                  )
              )
              mockNodeDat
          ]
    it "Should build a tree for a pointer declaration" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            OpTok Asterisk dummyLexDat,
            Ident "a" dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ PointerNode
              (VarNode "a" mockNodeDat)
              IntPointer
              Nothing
              mockNodeDat
          ]
    it "Should build a tree for a pointer declaration and assignment" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            OpTok Asterisk dummyLexDat,
            Ident "a" dummyLexDat,
            OpTok EqualSign dummyLexDat,
            OpTok Ampersand dummyLexDat,
            Ident "b" dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ PointerNode
              (VarNode "a" mockNodeDat)
              IntPointer
              ( Just
                  ( AssignmentNode
                      (VarNode "a" mockNodeDat)
                      (AddressOfNode "b" mockNodeDat)
                      Assignment
                      mockNodeDat
                  )
              )
              mockNodeDat
          ]
    it "Should build a tree for an array declaration" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            OpenBracket OpenSqBracket dummyLexDat,
            ConstInt 2 dummyLexDat,
            CloseBracket CloseSqBracket dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ ArrayNode
              ( ArrayDeclareNode
                  2
                  (VarNode "a" mockNodeDat)
                  IntArray
                  Nothing
                  mockNodeDat
              )
          ]
    it "Should build a tree for an array declaration and assignment" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            OpenBracket OpenSqBracket dummyLexDat,
            ConstInt 2 dummyLexDat,
            CloseBracket CloseSqBracket dummyLexDat,
            OpTok EqualSign dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            ConstInt 10 dummyLexDat,
            Separator Comma dummyLexDat,
            ConstInt 20 dummyLexDat,
            CloseBracket CloseBrace dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ ArrayNode
              ( ArrayDeclareNode
                  2
                  (VarNode "a" mockNodeDat)
                  IntArray
                  ( Just $
                      ArrayNode
                        ( ArrayItemsNode
                            (VarNode "a" mockNodeDat)
                            [ ArrayNode
                                ( ArraySingleItemNode
                                    (ConstantNode 10 mockNodeDat)
                                    mockNodeDat
                                ),
                              ArrayNode
                                ( ArraySingleItemNode
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
    it "Should build a tree for an array declaration and assignment with implicit length" $
      ( extractDeclarationTree
          [ Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            OpenBracket OpenSqBracket dummyLexDat,
            CloseBracket CloseSqBracket dummyLexDat,
            OpTok EqualSign dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            ConstInt 10 dummyLexDat,
            Separator Comma dummyLexDat,
            ConstInt 20 dummyLexDat,
            CloseBracket CloseBrace dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ ArrayNode
              ( ArrayDeclareNode
                  2
                  (VarNode "a" mockNodeDat)
                  IntArray
                  ( Just $
                      ArrayNode
                        ( ArrayItemsNode
                            (VarNode "a" mockNodeDat)
                            [ ArrayNode
                                ( ArraySingleItemNode
                                    (ConstantNode 10 mockNodeDat)
                                    mockNodeDat
                                ),
                              ArrayNode
                                ( ArraySingleItemNode
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
  describe "Throw errors on bad input" $ do
    -- This should probably be a warning not an error, like in gcc
    it "Should throw error if explicit and implicit array length differ" $
      ( extractDeclarationError
          [ Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            OpenBracket OpenSqBracket dummyLexDat,
            ConstInt 1 dummyLexDat,
            CloseBracket CloseSqBracket dummyLexDat,
            OpTok EqualSign dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            ConstInt 10 dummyLexDat,
            Separator Comma dummyLexDat,
            ConstInt 20 dummyLexDat,
            CloseBracket CloseBrace dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` SyntaxError (LengthMismatch (VarNode "a" mockNodeDat) 2 1)
