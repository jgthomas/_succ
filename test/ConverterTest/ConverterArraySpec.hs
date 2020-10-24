module ConverterTest.ConverterArraySpec
  ( converterArrayTest,
  )
where

import ConverterTest.TestUtility (extractSchema)
import Data.Map as M
import Test.Hspec
import TestUtility (mockNodeDat)
import Types.AST
import Types.AssemblySchema
import Types.Operator
import Types.Type
import Types.Variables

converterArrayTest :: IO ()
converterArrayTest = hspec $ do
  describe "Build assembly schemas for arrays" $ do
    it "Should create a schema for a function with local array declaration" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ArrayNode $
                            ArrayDeclareNode
                              2
                              (VarNode "a" mockNodeDat)
                              (IntArray 2)
                              Nothing
                              mockNodeDat,
                          ReturnNode
                            (ConstantNode 190 mockNodeDat)
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        SkipSchema
                        Local
                        (IntArray 2),
                      StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 190)
                    ]
              )
          ]
    it "Should create a schema for global array declaration" $
      ( extractSchema
          ( ProgramNode
              [ DeclarationNode
                  (VarNode "a" mockNodeDat)
                  (IntArray 2)
                  Nothing
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (MultiValue $ M.fromList [(0, 0), (1, 0)]))
              SkipSchema
              Global
              (IntArray 2),
            SkipSchema
          ]
    it "Should create a schema for a function with local array declaration with assignment" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ArrayNode $
                            ArrayDeclareNode
                              2
                              (VarNode "a" mockNodeDat)
                              (IntArray 2)
                              ( Just $ ArrayNode $
                                  ArrayItemsNode
                                    (VarNode "a" mockNodeDat)
                                    [ ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 20 mockNodeDat)
                                          mockNodeDat,
                                      ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 30 mockNodeDat)
                                          mockNodeDat
                                    ]
                                    mockNodeDat
                              )
                              mockNodeDat,
                          ReturnNode
                            (ConstantNode 190 mockNodeDat)
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            ArrayItemsSchema
                              24
                              [ StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 20)
                                    Local,
                                StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 30)
                                    Local
                              ]
                        )
                        Local
                        (IntArray 2),
                      StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 190)
                    ]
              )
          ]
    it "Should create a schema for a function with local array access" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ArrayNode $
                            ArrayDeclareNode
                              2
                              (VarNode "a" mockNodeDat)
                              (IntArray 2)
                              ( Just $ ArrayNode $
                                  ArrayItemsNode
                                    (VarNode "a" mockNodeDat)
                                    [ ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 20 mockNodeDat)
                                          mockNodeDat,
                                      ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 30 mockNodeDat)
                                          mockNodeDat
                                    ]
                                    mockNodeDat
                              )
                              mockNodeDat,
                          ReturnNode
                            ( ArrayNode $
                                ArrayItemAccess
                                  1
                                  (VarNode "a" mockNodeDat)
                                  mockNodeDat
                            )
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            ArrayItemsSchema
                              24
                              [ StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 20)
                                    Local,
                                StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 30)
                                    Local
                              ]
                        )
                        Local
                        (IntArray 2),
                      StatementSchema $
                        ReturnSchema
                          (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                    ]
              )
          ]
    it "Should create a schema for a function with local array index assignment" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ArrayNode $
                            ArrayDeclareNode
                              1
                              (VarNode "a" mockNodeDat)
                              (IntArray 1)
                              ( Just $ ArrayNode $
                                  ArrayItemsNode
                                    (VarNode "a" mockNodeDat)
                                    [ ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 20 mockNodeDat)
                                          mockNodeDat
                                    ]
                                    mockNodeDat
                              )
                              mockNodeDat,
                          ( ArrayNode $
                              ArrayAssignPosNode
                                ( ArrayNode $
                                    ArrayItemAssign
                                      0
                                      (VarNode "a" mockNodeDat)
                                      mockNodeDat
                                )
                                (ConstantNode 30 mockNodeDat)
                                Assignment
                                mockNodeDat
                          ),
                          ReturnNode
                            ( ArrayNode $
                                ArrayItemAccess
                                  0
                                  (VarNode "a" mockNodeDat)
                                  mockNodeDat
                            )
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            ArrayItemsSchema
                              16
                              [ StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 20)
                                    Local
                              ]
                        )
                        Local
                        (IntArray 1),
                      ( StatementSchema $
                          AssignmentSchema
                            (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                            (ExpressionSchema $ LiteralSchema 30)
                            Local
                      ),
                      ( StatementSchema
                          ( ReturnSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                          )
                      )
                    ]
              )
          ]
    it "Should create a schema for a function with local array index plus equals assignment" $
      ( extractSchema
          ( ProgramNode
              [ FunctionNode
                  IntVar
                  "main"
                  []
                  ( Just $
                      CompoundStmtNode
                        [ ArrayNode $
                            ArrayDeclareNode
                              2
                              (VarNode "a" mockNodeDat)
                              (IntArray 2)
                              ( Just $ ArrayNode $
                                  ArrayItemsNode
                                    (VarNode "a" mockNodeDat)
                                    [ ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 20 mockNodeDat)
                                          mockNodeDat,
                                      ArrayNode $
                                        ArraySingleItemNode
                                          (ConstantNode 100 mockNodeDat)
                                          mockNodeDat
                                    ]
                                    mockNodeDat
                              )
                              mockNodeDat,
                          ( ArrayNode $
                              ArrayAssignPosNode
                                ( ArrayNode $
                                    ArrayItemAssign
                                      1
                                      (VarNode "a" mockNodeDat)
                                      mockNodeDat
                                )
                                ( BinaryNode
                                    ( ArrayNode $
                                        ArrayItemAssign
                                          1
                                          (VarNode "a" mockNodeDat)
                                          mockNodeDat
                                    )
                                    (ConstantNode 30 mockNodeDat)
                                    Plus
                                    mockNodeDat
                                )
                                (BinaryOp Plus)
                                mockNodeDat
                          ),
                          ReturnNode
                            ( ArrayNode $
                                ArrayItemAccess
                                  1
                                  (VarNode "a" mockNodeDat)
                                  mockNodeDat
                            )
                            mockNodeDat
                        ]
                        mockNodeDat
                  )
                  mockNodeDat
              ]
          )
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        ( StatementSchema $
                            ArrayItemsSchema
                              24
                              [ StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 20)
                                    Local,
                                StatementSchema $
                                  AssignmentSchema
                                    (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                                    (ExpressionSchema $ LiteralSchema 100)
                                    Local
                              ]
                        )
                        Local
                        (IntArray 2),
                      ( StatementSchema $
                          AssignmentSchema
                            (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                            ( ExpressionSchema $
                                BinarySchema
                                  (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                                  ( ExpressionSchema $
                                      BinarySchema
                                        (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                                        (ExpressionSchema $ LiteralSchema 30)
                                        Plus
                                        (LocalLabel 3)
                                        (LocalLabel 4)
                                  )
                                  Plus
                                  (LocalLabel 1)
                                  (LocalLabel 2)
                            )
                            Local
                      ),
                      ( StatementSchema
                          ( ReturnSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-16) (-8) 24) UntrackedValue)
                          )
                      )
                    ]
              )
          ]
