module ParserTest.ParserFunctionSpec
  ( spec,
  )
where

import ParserTest.TestUtility (extractFunctionError, extractFunctionTree)
import Test.Hspec
import TestUtility (mockNodeDat)
import Types.AST
import Types.Error
import Types.Tokens
import Types.Type

spec :: Spec
spec = do
  describe "Build abstract syntax trees for functions" $ do
    it "Should build a tree for a function declaration" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            Ident "dog" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntVar
              "dog"
              []
              Nothing
              mockNodeDat
          ]
    it "Should build a tree for a function declaration returning a pointer" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            OpTok Asterisk dummyLexDat,
            Ident "cat" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntPointer
              "cat"
              []
              Nothing
              mockNodeDat
          ]
    it "Should build a tree for a function definition" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            Ident "main" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            CloseBracket CloseBrace dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntVar
              "main"
              []
              (Just $ CompoundStmtNode [] mockNodeDat)
              mockNodeDat
          ]
    it "Should build a tree for a function with arguments" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            Ident "main" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            Separator Comma dummyLexDat,
            Keyword Int dummyLexDat,
            OpTok Asterisk dummyLexDat,
            Ident "b" dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            CloseBracket CloseBrace dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntVar
              "main"
              [ ParamNode
                  IntVar
                  (VarNode "a" mockNodeDat)
                  mockNodeDat,
                ParamNode
                  IntPointer
                  (VarNode "b" mockNodeDat)
                  mockNodeDat
              ]
              (Just $ CompoundStmtNode [] mockNodeDat)
              mockNodeDat
          ]
    it "Should build a tree for a function returning a pointer" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            OpTok Asterisk dummyLexDat,
            Ident "dog" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            Keyword Int dummyLexDat,
            Ident "a" dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            CloseBracket CloseBrace dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntPointer
              "dog"
              [ ParamNode
                  IntVar
                  (VarNode "a" mockNodeDat)
                  mockNodeDat
              ]
              (Just $ CompoundStmtNode [] mockNodeDat)
              mockNodeDat
          ]
    it "Should build a tree for a function with body statements" $
      ( extractFunctionTree
          [ Keyword Int dummyLexDat,
            Ident "main" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            Keyword Return dummyLexDat,
            ConstInt 2 dummyLexDat,
            Separator SemiColon dummyLexDat,
            CloseBracket CloseBrace dummyLexDat
          ]
      )
        `shouldBe` ProgramNode
          [ FunctionNode
              IntVar
              "main"
              []
              ( Just $
                  CompoundStmtNode
                    [ ReturnNode
                        (ConstantNode 2 mockNodeDat)
                        mockNodeDat
                    ]
                    mockNodeDat
              )
              mockNodeDat
          ]
  describe "Throw errors on bad input" $ do
    it "Should throw error on missing function identifier" $
      ( extractFunctionError
          [ Keyword Int dummyLexDat,
            Separator Comma dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            OpenBracket OpenBrace dummyLexDat,
            CloseBracket CloseBrace dummyLexDat
          ]
      )
        `shouldBe` SyntaxError (NonValidIdentifier $ Keyword Int dummyLexDat)
    it "Should throw error on unfinished function" $
      (extractFunctionError [Keyword Int dummyLexDat, Ident "a" dummyLexDat])
        `shouldBe` ParserError (LexDataError [Keyword Int dummyLexDat, Ident "a" dummyLexDat])
    it "Should throw an error on invalid type" $
      ( extractFunctionError
          [ Separator SemiColon dummyLexDat,
            Ident "dog" dummyLexDat,
            OpenBracket OpenParen dummyLexDat,
            CloseBracket CloseParen dummyLexDat,
            Separator SemiColon dummyLexDat
          ]
      )
        `shouldBe` SyntaxError (BadType $ Separator SemiColon dummyLexDat)
