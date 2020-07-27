
module ConverterTest.ConverterTestSpec (converterTest) where


import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility               (mockNodeDat)
import Types.AssemblySchema
import Types.AST
import Types.Operator
import Types.Type
import Types.Variables


converterTest :: IO ()
converterTest = hspec $ do
        describe "Build assembly schemas from syntax trees" $ do

                it "Should create a global declaration schema" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    SkipSchema
                    Global
                    IntVar
                   ]

                it "Should create a schema for a global declaration with assignment" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "a" mockNodeDat)
                                     (ConstantNode 10 mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    (StatementSchema $ AssignmentSchema
                     (VariableSchema (GlobalVar "_a1" 0))
                     (LiteralSchema 10)
                     Global
                    )
                    Global
                    IntVar
                   ]

                it "Should create a schema for a global pointer declaration" $
                  (extractSchema $ ProgramNode [
                                    PointerNode
                                    (VarNode "a" mockNodeDat)
                                    IntPointer
                                    Nothing
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntPointer
                   ]

                it "Should create a schema for a global pointer declaration with assignment" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat,
                                    PointerNode
                                    (VarNode "b" mockNodeDat)
                                    IntPointer
                                    (Just $ AssignmentNode
                                     (VarNode "b" mockNodeDat)
                                     (AddressOfNode "a" mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntVar,
                    DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0))
                    (StatementSchema $ AssignmentSchema
                     (VariableSchema (GlobalVar "_b2" 0))
                     (AddressOfSchema $ VariableSchema (GlobalVar "_a1" 0))
                     Global
                    )
                    Global
                    IntPointer
                   ]

                it "Should create a schema for a pointer dereference" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat,
                                    PointerNode
                                    (VarNode "b" mockNodeDat)
                                    IntPointer
                                    (Just $ AssignmentNode
                                     (VarNode "b" mockNodeDat)
                                     (AddressOfNode "a" mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat,
                                    DeclarationNode
                                    (VarNode "c" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "c" mockNodeDat)
                                     (DereferenceNode "b" mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntVar,
                    DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0))
                    (StatementSchema $ AssignmentSchema
                     (VariableSchema (GlobalVar "_b2" 0))
                     (AddressOfSchema $ VariableSchema (GlobalVar "_a1" 0))
                     Global
                    )
                    Global
                    IntPointer,
                    DeclarationSchema
                    (ExpressionSchema $ VariableSchema $ GlobalVar "_c3" 0)
                    (StatementSchema $ AssignmentSchema
                     (VariableSchema $ GlobalVar "_c3" 0)
                     (DereferenceSchema $ VariableSchema $ GlobalVar "_b2" 0)
                     Global
                    )
                    Global
                    IntVar
                   ]

                it "Should create a schema for assigning to a derferenced pointer" $
                  (extractSchema $ ProgramNode
                                   [FunctionNode
                                    IntVar
                                    "main"
                                    []
                                    (Just $ CompoundStmtNode
                                     [DeclarationNode
                                      (VarNode "a" mockNodeDat)
                                      IntVar
                                      Nothing
                                      mockNodeDat,
                                      PointerNode
                                      (VarNode "b" mockNodeDat)
                                      IntPointer
                                      (Just $ AssignmentNode
                                       (VarNode "b" mockNodeDat)
                                       (AddressOfNode "a" mockNodeDat)
                                       Assignment
                                       mockNodeDat
                                      )
                                      mockNodeDat,
                                      AssignDereferenceNode
                                      (VarNode "b" mockNodeDat)
                                      (ConstantNode 20 mockNodeDat)
                                      Assignment
                                      mockNodeDat
                                     ]
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [DeclarationSchema
                      (ExpressionSchema $ VariableSchema $ LocalVar (-16) 0 16)
                      SkipSchema
                      Local
                      IntVar,
                      DeclarationSchema
                      (ExpressionSchema $ VariableSchema $ LocalVar (-24) 0 24)
                      (StatementSchema $ AssignmentSchema
                       (VariableSchema $ LocalVar (-24) 0 24)
                       (AddressOfSchema $ VariableSchema $ LocalVar (-16) 0 16)
                       Local
                      )
                      Local
                      IntPointer,
                      StatementSchema $ AssignmentSchema
                      (VariableSchema $ LocalVar (-24) 0 24)
                      (LiteralSchema 20)
                      Local
                     ]
                    )
                   ]

                it "Should create a schema for a function with local declaration" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     SkipSchema
                     Local
                     IntVar,
                     StatementSchema $ ReturnSchema (LiteralSchema 190)
                    ]
                   )
                  ]

                it "Should create a schema for a function with an expression statement" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ExprStmtNode
                                     (BinaryNode
                                      (ConstantNode 2 mockNodeDat)
                                      (ConstantNode 2 mockNodeDat)
                                      Plus
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [ExpressionSchema $ BinarySchema
                     (LiteralSchema 2)
                     (LiteralSchema 2)
                     Plus
                     (LocalLabel 1)
                     (LocalLabel 2)
                    ]
                   )
                  ]

                it "Should create a schema for a function with binary operation" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
                                     (Just $ AssignmentNode
                                      (VarNode "a" mockNodeDat)
                                      (BinaryNode
                                       (ConstantNode 10 mockNodeDat)
                                       (ConstantNode 10 mockNodeDat)
                                       Plus
                                       mockNodeDat
                                      )
                                      Assignment
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     ReturnNode
                                     (VarNode "a" mockNodeDat)
                                     mockNodeDat
                                    ]
                                    mockNodeDat
                                   )
                                   mockNodeDat
                                  ]
                                 )
                  )
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema $ LocalVar (-16) 0 16)
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 10)
                       Plus
                       (LocalLabel 1)
                       (LocalLabel 2)
                      )
                      Local
                     )
                     Local
                     IntVar,
                     StatementSchema
                     (ReturnSchema
                      (VariableSchema $ LocalVar (-16) 0 16)
                     )
                    ]
                   )
                  ]

                it "Should create a schema for a function with ternary operation" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
                                     (Just $ AssignmentNode
                                      (VarNode "a" mockNodeDat)
                                      (TernaryNode
                                       (BinaryNode
                                        (ConstantNode 12 mockNodeDat)
                                        (ConstantNode 10 mockNodeDat)
                                        GreaterThan
                                        mockNodeDat
                                       )
                                       (ConstantNode 90 mockNodeDat)
                                       (ConstantNode 100 mockNodeDat)
                                       mockNodeDat
                                      )
                                      Assignment
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     ReturnNode
                                     (VarNode "a" mockNodeDat)
                                     mockNodeDat
                                    ]
                                    mockNodeDat
                                   )
                                   mockNodeDat
                                  ]
                                 )
                  )
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema $ LocalVar (-16) 0 16)
                      (TernarySchema
                       (BinarySchema
                        (LiteralSchema 12)
                        (LiteralSchema 10)
                        GreaterThan
                        (LocalLabel 1)
                        (LocalLabel 2)
                       )
                       (LiteralSchema 90)
                       (LiteralSchema 100)
                       (LocalLabel 3)
                       (LocalLabel 4)
                      )
                      Local
                     )
                     Local
                     IntVar,
                     StatementSchema
                     (ReturnSchema
                      (VariableSchema $ LocalVar (-16) 0 16)
                     )
                    ]
                   )
                  ]

                it "Should create a schema for a function returning a unary negation" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
                                     Nothing
                                     mockNodeDat,
                                     ReturnNode
                                     (UnaryNode
                                      (VarNode "a" mockNodeDat)
                                      (Unary Negate)
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     SkipSchema
                     Local
                     IntVar,
                     StatementSchema
                     (ReturnSchema
                      (UnarySchema
                       (VariableSchema $ LocalVar (-16) 0 16)
                       (Unary Negate)
                      )
                     )
                    ]
                   )
                  ]

                it "Should create a schema for a function with local declaration with assignment" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [DeclarationNode
                                     (VarNode "a" mockNodeDat)
                                     IntVar
                                     (Just $ AssignmentNode
                                      (VarNode "a" mockNodeDat)
                                      (ConstantNode 100 mockNodeDat)
                                      Assignment
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     ReturnNode
                                     (VarNode "a" mockNodeDat)
                                     mockNodeDat
                                    ]
                                    mockNodeDat
                                   )
                                   mockNodeDat
                                  ]
                                 )
                  )
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema (LocalVar (-16) 0 16))
                      (LiteralSchema 100)
                      Local
                     )
                     Local
                     IntVar,
                     StatementSchema $ ReturnSchema
                     (VariableSchema (LocalVar (-16) 0 16))
                    ]
                   )
                  ]

                it "Should create a schema for a function with local array declaration" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ArrayNode $ ArrayDeclareNode
                                     2
                                     (VarNode "a" mockNodeDat)
                                     IntArray
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     SkipSchema
                     Local
                     IntArray,
                     StatementSchema $ ReturnSchema (LiteralSchema 190)
                    ]
                   )
                  ]

                it "Should create a schema for a function with local array declaration with assignment" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ArrayNode $ ArrayDeclareNode
                                     2
                                     (VarNode "a" mockNodeDat)
                                     IntArray
                                     (Just $ ArrayNode $ ArrayItemsNode
                                      (VarNode "a" mockNodeDat)
                                      [ArrayNode $ ArraySingleItemNode
                                       (ConstantNode 20 mockNodeDat)
                                       mockNodeDat,
                                       ArrayNode $ ArraySingleItemNode
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (ExpressionSchema $ ArrayItemsSchema
                      24
                      [AssignmentSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                       (LiteralSchema 20)
                       Local,
                       AssignmentSchema
                       (VariableSchema (LocalVar (-16) (-8) 24))
                       (LiteralSchema 30)
                       Local
                      ]
                     )
                     Local
                     IntArray,
                     StatementSchema $ ReturnSchema (LiteralSchema 190)
                    ]
                   )
                  ]

                it "Should create a schema using SkipSchema for a function repeatedly declared" $
                  (extractSchema $ ProgramNode [
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat,
                                    FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    Nothing
                                    mockNodeDat,
                                    FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    Nothing
                                    mockNodeDat,
                                    FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    (Just $ CompoundStmtNode
                                     [ReturnNode
                                      (ConstantNode 10 mockNodeDat)
                                      mockNodeDat
                                     ]
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0 ))
                    SkipSchema
                    Global
                    IntVar,
                    SkipSchema,
                    SkipSchema,
                    FunctionSchema
                    "dog"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ReturnSchema (LiteralSchema 10)]
                    )
                   ]

                it "Should create a schema for a function with local array access" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ArrayNode $ ArrayDeclareNode
                                     2
                                     (VarNode "a" mockNodeDat)
                                     IntArray
                                     (Just $ ArrayNode $ ArrayItemsNode
                                      (VarNode "a" mockNodeDat)
                                      [ArrayNode $ ArraySingleItemNode
                                       (ConstantNode 20 mockNodeDat)
                                       mockNodeDat,
                                       ArrayNode $ ArraySingleItemNode
                                       (ConstantNode 30 mockNodeDat)
                                       mockNodeDat
                                      ]
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     ReturnNode
                                     (ArrayNode $ ArrayItemAccess
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (ExpressionSchema $ ArrayItemsSchema
                      24
                      [AssignmentSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                       (LiteralSchema 20)
                       Local,
                       AssignmentSchema
                       (VariableSchema (LocalVar (-16) (-8) 24))
                       (LiteralSchema 30)
                       Local
                      ]
                     )
                     Local
                     IntArray,
                     StatementSchema $
                     ReturnSchema
                     (VariableSchema (LocalVar (-16) (-8) 24))
                    ]
                   )
                  ]

                it "Should create a schema for a function with local array index assignment" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ArrayNode $ ArrayDeclareNode
                                     1
                                     (VarNode "a" mockNodeDat)
                                     IntArray
                                     (Just $ ArrayNode $ ArrayItemsNode
                                      (VarNode "a" mockNodeDat)
                                      [ArrayNode $ ArraySingleItemNode
                                       (ConstantNode 20 mockNodeDat)
                                       mockNodeDat
                                      ]
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     (ArrayNode $ ArrayAssignPosNode
                                      (ArrayNode $ ArrayItemAssign
                                       0
                                       (VarNode "a" mockNodeDat)
                                       mockNodeDat
                                      )
                                      (ConstantNode 30 mockNodeDat)
                                      Assignment
                                      mockNodeDat
                                     ),
                                     ReturnNode
                                     (ArrayNode $ ArrayItemAccess
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (ExpressionSchema $ ArrayItemsSchema
                      16
                      [AssignmentSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                       (LiteralSchema 20)
                       Local
                      ]
                     )
                     Local
                     IntArray,
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema (LocalVar (-16) 0 16))
                      (LiteralSchema 30)
                      Local
                     ),
                     (StatementSchema
                      (ReturnSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                      )
                     )
                    ]
                   )
                  ]

                it "Should create a schema for a function with local array index plus equals assignment" $
                  (extractSchema (ProgramNode
                                  [FunctionNode
                                   IntVar
                                   "main"
                                   []
                                   (Just $ CompoundStmtNode
                                    [ArrayNode $ ArrayDeclareNode
                                     1
                                     (VarNode "a" mockNodeDat)
                                     IntArray
                                     (Just $ ArrayNode $ ArrayItemsNode
                                      (VarNode "a" mockNodeDat)
                                      [ArrayNode $ ArraySingleItemNode
                                       (ConstantNode 20 mockNodeDat)
                                       mockNodeDat
                                      ]
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     (ArrayNode $ ArrayAssignPosNode
                                      (ArrayNode $ ArrayItemAssign
                                       0
                                       (VarNode "a" mockNodeDat)
                                       mockNodeDat
                                      )
                                      (BinaryNode
                                       (VarNode "a" mockNodeDat)
                                       (ConstantNode 30 mockNodeDat)
                                       Plus
                                       mockNodeDat
                                      )
                                      (BinaryOp Plus)
                                      mockNodeDat
                                     ),
                                     ReturnNode
                                     (ArrayNode $ ArrayItemAccess
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
                  `shouldBe`
                  ProgramSchema
                  [FunctionSchema
                   "main"
                   (StatementSchema $ CompoundStatementSchema
                    [DeclarationSchema
                     (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16))
                     (ExpressionSchema $ ArrayItemsSchema
                      16
                      [AssignmentSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                       (LiteralSchema 20)
                       Local
                      ]
                     )
                     Local
                     IntArray,
                     (StatementSchema $ AssignmentSchema
                      (VariableSchema (LocalVar (-16) 0 16))
                      (BinarySchema
                       (VariableSchema $ LocalVar (-16) 0 16)
                       (BinarySchema
                        (VariableSchema $ LocalVar (-16) 0 16)
                        (LiteralSchema 30)
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
                     (StatementSchema
                      (ReturnSchema
                       (VariableSchema (LocalVar (-16) 0 16))
                      )
                     )
                    ]
                   )
                  ]

                it "Should build a schema for a function call" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "dog"
                                                []
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [ReturnNode
                                                  (FuncCallNode "dog" [] mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [SkipSchema,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ReturnSchema
                      (FunctionCallSchema
                       "dog"
                       []
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a function call with arguments" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "dog"
                                                [ParamNode
                                                 IntVar
                                                 (VarNode "a" mockNodeDat)
                                                 mockNodeDat
                                                ]
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [ReturnNode
                                                  (FuncCallNode
                                                   "dog"
                                                   [ArgNode
                                                    (ConstantNode 29 mockNodeDat)
                                                    mockNodeDat
                                                   ]
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [SkipSchema,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ReturnSchema
                      (FunctionCallSchema
                       "dog"
                       [(LiteralSchema 29)]
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a basic if statement" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [IfNode
                                                  (BinaryNode
                                                   (ConstantNode 10 mockNodeDat)
                                                   (ConstantNode 90 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  (ReturnNode
                                                   (ConstantNode 22 mockNodeDat)
                                                   mockNodeDat
                                                  )
                                                  Nothing
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (ConstantNode 66 mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ IfSchema
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 90)
                       LessThan
                       (LocalLabel 3)
                       (LocalLabel 4)
                      )
                      (ReturnSchema
                       (LiteralSchema 22)
                      )
                      SkipSchema
                      (LocalLabel 1)
                      (LocalLabel 2),
                      (StatementSchema $ ReturnSchema
                       (LiteralSchema 66)
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a while statement" $
                  (extractSchema $ ProgramNode [DeclarationNode
                                                (VarNode "a" mockNodeDat)
                                                IntVar
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [WhileNode
                                                  (BinaryNode
                                                   (ConstantNode 10 mockNodeDat)
                                                   (ConstantNode 90 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  (CompoundStmtNode
                                                   [AssignmentNode
                                                    (VarNode "a" mockNodeDat)
                                                    (ConstantNode 100 mockNodeDat)
                                                    Assignment
                                                    mockNodeDat
                                                   ]
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (VarNode "a" mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema (VariableSchema $ GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntVar,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ WhileSchema
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 90)
                       LessThan
                       (LocalLabel 4)
                       (LocalLabel 5)
                      )
                      (CompoundStatementSchema
                       [StatementSchema $ AssignmentSchema
                        (VariableSchema $ GlobalVar "_a1" 0)
                        (LiteralSchema 100)
                        Local
                       ]
                      )
                      (LocalLabel 2)
                      (LocalLabel 3),
                      (StatementSchema $ ReturnSchema
                       (VariableSchema $ GlobalVar "_a1" 0)
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a do-while statement" $
                  (extractSchema $ ProgramNode [DeclarationNode
                                                (VarNode "a" mockNodeDat)
                                                IntVar
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [DoWhileNode
                                                  (CompoundStmtNode
                                                   [AssignmentNode
                                                    (VarNode "a" mockNodeDat)
                                                    (ConstantNode 100 mockNodeDat)
                                                    Assignment
                                                    mockNodeDat
                                                   ]
                                                   mockNodeDat
                                                  )
                                                  (BinaryNode
                                                   (ConstantNode 10 mockNodeDat)
                                                   (ConstantNode 90 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (VarNode "a" mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema (VariableSchema $ GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntVar,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ DoWhileSchema
                      (CompoundStatementSchema
                       [StatementSchema $ AssignmentSchema
                        (VariableSchema $ GlobalVar "_a1" 0)
                        (LiteralSchema 100)
                        Local
                       ]
                      )
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 90)
                       LessThan
                       (LocalLabel 5)
                       (LocalLabel 6)
                      )
                      (LocalLabel 2)
                      (LocalLabel 3)
                      (LocalLabel 4),
                      (StatementSchema $ ReturnSchema
                       (VariableSchema $ GlobalVar "_a1" 0)
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a for statement" $
                  (extractSchema $ ProgramNode [DeclarationNode
                                                (VarNode "a" mockNodeDat)
                                                IntVar
                                                Nothing
                                                mockNodeDat,
                                                FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [ForLoopNode
                                                  (DeclarationNode
                                                   (VarNode "i" mockNodeDat)
                                                   IntVar
                                                   (Just $ AssignmentNode
                                                    (VarNode "i" mockNodeDat)
                                                    (ConstantNode 1 mockNodeDat)
                                                    Assignment
                                                    mockNodeDat
                                                   )
                                                   mockNodeDat
                                                  )
                                                  (BinaryNode
                                                   (VarNode "i" mockNodeDat)
                                                   (ConstantNode 10 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  (BinaryNode
                                                   (VarNode "i" mockNodeDat)
                                                   (ConstantNode 1 mockNodeDat)
                                                   Plus
                                                   mockNodeDat
                                                  )
                                                  (CompoundStmtNode
                                                   [AssignmentNode
                                                    (VarNode "a" mockNodeDat)
                                                    (ConstantNode 100 mockNodeDat)
                                                    Assignment
                                                    mockNodeDat
                                                   ]
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (VarNode "a" mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [DeclarationSchema
                    (ExpressionSchema (VariableSchema $ GlobalVar "_a1" 0))
                    SkipSchema
                    Global
                    IntVar,
                    FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ ForSchema
                      (DeclarationSchema
                       (ExpressionSchema (VariableSchema $ LocalVar (-16) 0 16))
                       (StatementSchema $ AssignmentSchema
                        (VariableSchema $ LocalVar (-16) 0 16)
                        (LiteralSchema 1)
                        Local
                       )
                       Local
                       IntVar
                      )
                      (BinarySchema
                       (VariableSchema $ LocalVar (-16) 0 16)
                       (LiteralSchema 10)
                       LessThan
                       (LocalLabel 5)
                       (LocalLabel 6)
                      )
                      (BinarySchema
                       (VariableSchema $ LocalVar (-16) 0 16)
                       (LiteralSchema 1)
                       Plus
                       (LocalLabel 7)
                       (LocalLabel 8)
                      )
                      (CompoundStatementSchema
                       [StatementSchema $ AssignmentSchema
                        (VariableSchema $ GlobalVar "_a1" 0)
                        (LiteralSchema 100)
                        Local
                       ]
                      )
                      (LocalLabel 2)
                      (LocalLabel 3)
                      (LocalLabel 4),
                      (StatementSchema $ ReturnSchema
                       (VariableSchema $ GlobalVar "_a1" 0)
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a loop with a break" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [WhileNode
                                                  (BinaryNode
                                                   (ConstantNode 10 mockNodeDat)
                                                   (ConstantNode 90 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  (CompoundStmtNode
                                                   [BreakNode mockNodeDat]
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (ConstantNode 10 mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ WhileSchema
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 90)
                       LessThan
                       (LocalLabel 3)
                       (LocalLabel 4)
                      )
                      (CompoundStatementSchema
                       [StatementSchema $ BreakSchema (LocalLabel 2)]
                      )
                      (LocalLabel 1)
                      (LocalLabel 2),
                      (StatementSchema $ ReturnSchema
                       (LiteralSchema 10)
                      )
                     ]
                    )
                   ]

                it "Should build a schema for a loop with a continue" $
                  (extractSchema $ ProgramNode [FunctionNode
                                                IntVar
                                                "main"
                                                []
                                                (Just $ CompoundStmtNode
                                                 [WhileNode
                                                  (BinaryNode
                                                   (ConstantNode 10 mockNodeDat)
                                                   (ConstantNode 90 mockNodeDat)
                                                   LessThan
                                                   mockNodeDat
                                                  )
                                                  (CompoundStmtNode
                                                   [ContinueNode mockNodeDat]
                                                   mockNodeDat
                                                  )
                                                  mockNodeDat,
                                                  ReturnNode
                                                  (ConstantNode 10 mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 mockNodeDat
                                                )
                                                mockNodeDat
                                               ]
                  )
                  `shouldBe`
                  ProgramSchema
                   [FunctionSchema
                    "main"
                    (StatementSchema $ CompoundStatementSchema
                     [StatementSchema $ WhileSchema
                      (BinarySchema
                       (LiteralSchema 10)
                       (LiteralSchema 90)
                       LessThan
                       (LocalLabel 3)
                       (LocalLabel 4)
                      )
                      (CompoundStatementSchema
                       [StatementSchema $ ContinueSchema (LocalLabel 1)]
                      )
                      (LocalLabel 1)
                      (LocalLabel 2),
                      (StatementSchema $ ReturnSchema
                       (LiteralSchema 10)
                      )
                     ]
                    )
                   ]
