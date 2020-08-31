

module ConverterTest.ConverterErrorSpec (converterErrorTest) where


import Test.Hspec

import ConverterTest.TestUtility (extractError)
import TestUtility               (mockNodeDat)
import Types.AST
import Types.Error
import Types.Operator
import Types.Type


converterErrorTest :: IO ()
converterErrorTest = hspec $ do
        describe "Check abstract syntax tree for scope errors" $ do

                it "Should throw error if attempting to use an undeclared variable" $
                  (extractError $ (ProgramNode
                                   [FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    (Just $ CompoundStmtNode
                                     [ReturnNode
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
                  ScopeError (UnrecognisedNode (VarNode "a" mockNodeDat))

                it "Should throw error if accessing the address of a variable in a register" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (AddressOfNode "a" mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (Unaddressable (AddressOfNode "a" mockNodeDat))

                it "Should throw an error if dereferencing a pointer that doesn't exist" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (DereferenceNode "a" mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (UnrecognisedNode (DereferenceNode "a" mockNodeDat))


                it "Should throw an error if parameter and argument counts don't match" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat,
                                   ParamNode
                                   IntVar
                                   (VarNode "b" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (VarNode "a" mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
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
                                      (ConstantNode 2 mockNodeDat)
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
                  )
                  `shouldBe`
                  ScopeError (MisMatchNode 2 (FuncCallNode
                                              "dog"
                                              [(ArgNode
                                                (ConstantNode 2 mockNodeDat))
                                               mockNodeDat
                                              ]
                                              mockNodeDat))

                it "Should throw an error if a function is defined twice" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (ConstantNode 2 mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat,
                                  FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (ConstantNode 12 mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (DoubleDefinedNode $
                              (FunctionNode
                               IntVar
                               "dog"
                                []
                                (Just $ CompoundStmtNode
                                 [ReturnNode
                                  (ConstantNode 12 mockNodeDat)
                                  mockNodeDat
                                 ]
                                 mockNodeDat
                                )
                                mockNodeDat
                              )
                             )

                it "Should throw an error if parameter counts differ between declarations" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat,
                                   ParamNode
                                   IntVar
                                   (VarNode "b" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  Nothing
                                  mockNodeDat,
                                  FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (ConstantNode 12 mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (MisMatchNode 2 (FunctionNode
                                             IntVar
                                             "dog"
                                             [ParamNode
                                              IntVar
                                              (VarNode "a" mockNodeDat)
                                              mockNodeDat
                                             ]
                                             (Just $ CompoundStmtNode
                                              [ReturnNode
                                               (ConstantNode 12 mockNodeDat)
                                               mockNodeDat
                                              ]
                                              mockNodeDat
                                             )
                                             mockNodeDat)
                             )

                it "Should throw an error if function identifier already used for a global variable" $
                  (extractError (ProgramNode
                                 [DeclarationNode
                                  (VarNode "dog" mockNodeDat)
                                  IntVar
                                  Nothing
                                  mockNodeDat,
                                  FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  (Just $ CompoundStmtNode
                                   [ReturnNode
                                    (ConstantNode 12 mockNodeDat)
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (DoubleDefinedNode (FunctionNode
                                                 IntVar
                                                 "dog"
                                                 [ParamNode
                                                  IntVar
                                                  (VarNode "a" mockNodeDat)
                                                  mockNodeDat
                                                 ]
                                                 (Just $ CompoundStmtNode
                                                  [ReturnNode
                                                   (ConstantNode 12 mockNodeDat)
                                                   mockNodeDat
                                                  ]
                                                  mockNodeDat
                                                 )
                                                 mockNodeDat
                                                )
                             )

                it "Should throw an error if global identifier already used for a function" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  Nothing
                                  mockNodeDat,
                                  DeclarationNode
                                  (VarNode "dog" mockNodeDat)
                                  IntVar
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (DoubleDeclaredNode (DeclarationNode
                                                  (VarNode "dog" mockNodeDat)
                                                  IntVar
                                                  Nothing
                                                  mockNodeDat
                                                 )
                             )

                it "Should throw an error if two variables in same scope have the same identifier" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  (Just $ CompoundStmtNode
                                   [DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat,
                                    DeclarationNode
                                    (VarNode "a" mockNodeDat)
                                    IntVar
                                    Nothing
                                    mockNodeDat
                                   ]
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (DoubleDeclaredNode (DeclarationNode
                                                  (VarNode "a" mockNodeDat)
                                                  IntVar
                                                  Nothing
                                                  mockNodeDat
                                                 )
                             )

                it "Should throw an error if trying to assign to an already defined global" $
                  (extractError $ (ProgramNode
                                   [DeclarationNode
                                    (VarNode "dog" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "dog" mockNodeDat)
                                     (ConstantNode 12 mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat,
                                    AssignmentNode
                                    (VarNode "dog" mockNodeDat)
                                    (ConstantNode 22 mockNodeDat)
                                    Assignment
                                    mockNodeDat
                                   ]
                                  )
                  )
                  `shouldBe`
                  ScopeError (DoubleDefinedNode (AssignmentNode
                                                 (VarNode "dog" mockNodeDat)
                                                 (ConstantNode 22 mockNodeDat)
                                                 Assignment
                                                 mockNodeDat
                                                )
                             )

                it "Should throw an error if trying to redeclare an already defined global" $
                  (extractError $ (ProgramNode
                                   [DeclarationNode
                                    (VarNode "dog" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "dog" mockNodeDat)
                                     (ConstantNode 12 mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat,
                                    DeclarationNode
                                    (VarNode "dog" mockNodeDat)
                                    IntVar
                                    (Just $ AssignmentNode
                                     (VarNode "dog" mockNodeDat)
                                     (ConstantNode 15 mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                                  )
                  )
                  `shouldBe`
                  ScopeError (DoubleDefinedNode (AssignmentNode
                                                 (VarNode "dog" mockNodeDat)
                                                 (ConstantNode 15 mockNodeDat)
                                                 Assignment
                                                 mockNodeDat
                                                )
                             )

                it "Should throw error if attempting to assign to a variable not in scope" $
                  (extractError $ (ProgramNode
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
                                       (VarNode "b" mockNodeDat)
                                       Assignment
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
                  ScopeError (UnrecognisedNode (VarNode "b" mockNodeDat))

                it "Should throw an error if break node found with no defined break point" $
                  (extractError $ ProgramNode [BreakNode mockNodeDat])
                  `shouldBe`
                  ScopeError (UnexpectedNode (BreakNode mockNodeDat))

                it "Should throw an error if continue node found with no defined continue point" $
                  (extractError $ ProgramNode [ContinueNode mockNodeDat])
                  `shouldBe`
                  ScopeError (UnexpectedNode (ContinueNode mockNodeDat))

                it "Should throw error if trying to assign variable address to pointer that doesn't exist" $
                  (extractError $ (ProgramNode
                                   [PointerNode
                                    (VarNode "a" mockNodeDat)
                                    IntPointer
                                    (Just $ AssignmentNode
                                     (VarNode "a" mockNodeDat)
                                     (AddressOfNode "b" mockNodeDat)
                                     Assignment
                                     mockNodeDat
                                    )
                                    mockNodeDat
                                   ]
                                  )
                  )
                  `shouldBe`
                  ScopeError (UnrecognisedNode (AddressOfNode "b" mockNodeDat))

                it "Should throw error if an if test refers to a non-existent variable" $
                  (extractError (ProgramNode
                                 [IfNode
                                  (BinaryNode
                                   (VarNode "a" mockNodeDat)
                                   (ConstantNode 10 mockNodeDat)
                                   GreaterThan
                                   mockNodeDat
                                  )
                                  (ReturnNode
                                   (ConstantNode 10 mockNodeDat)
                                   mockNodeDat
                                  )
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (UnrecognisedNode (VarNode "a" mockNodeDat))

                it "Should throw error if an if body refers to a non-existent variable" $
                  (extractError (ProgramNode
                                 [IfNode
                                  (BinaryNode
                                   (ConstantNode 20 mockNodeDat)
                                   (ConstantNode 10 mockNodeDat)
                                   GreaterThan
                                   mockNodeDat
                                  )
                                  (ReturnNode
                                   (VarNode "b" mockNodeDat)
                                   mockNodeDat
                                  )
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  ScopeError (UnrecognisedNode (VarNode "b" mockNodeDat))

        describe "Check abstract syntax tree for type errors" $ do

                it "Should throw error if return type doesn't match function declaration" $
                  (extractError $ (ProgramNode
                                   [PointerNode
                                    (VarNode "a" mockNodeDat)
                                    IntPointer
                                    Nothing
                                    mockNodeDat,
                                    FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    (Just $ CompoundStmtNode
                                     [ReturnNode
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
                  TypeError (TypeMismatch
                             [IntVar]
                             [IntPointer]
                             (ReturnNode (VarNode "a" mockNodeDat) mockNodeDat)
                            )

                it "Should throw an error if types differ between function declarations" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntVar
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat,
                                   ParamNode
                                   IntPointer
                                   (VarNode "b" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  Nothing
                                  mockNodeDat,
                                  FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntPointer
                                   (VarNode "a" mockNodeDat)
                                   mockNodeDat,
                                   ParamNode
                                   IntVar
                                   (VarNode "b" mockNodeDat)
                                   mockNodeDat
                                  ]
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch
                             [IntVar, IntPointer]
                             [IntPointer, IntVar]
                             (FunctionNode
                             IntVar
                             "dog"
                             [ParamNode
                              IntPointer
                              (VarNode "a" mockNodeDat)
                              mockNodeDat,
                              ParamNode
                              IntVar
                              (VarNode "b" mockNodeDat)
                              mockNodeDat
                             ]
                             Nothing
                             mockNodeDat)
                            )

                it "Should throw an error if types differ between function declaration and calling" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  [ParamNode
                                   IntPointer
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
                                      (ConstantNode 2 mockNodeDat)
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
                  )
                  `shouldBe`
                  TypeError (TypeMismatch
                             [IntPointer]
                             [IntVar]
                             (FuncCallNode
                              "dog"
                              [ArgNode
                               (ConstantNode 2 mockNodeDat)
                               mockNodeDat
                              ]
                              mockNodeDat
                             )
                            )

                it "Should throw an error if types differ between variable declaration and assignment" $
                  (extractError (ProgramNode
                                 [DeclarationNode
                                  (VarNode "b" mockNodeDat)
                                  IntArray
                                  (Just $ AssignmentNode
                                   (VarNode "b" mockNodeDat)
                                   (ConstantNode 10 mockNodeDat)
                                   Assignment
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch [IntArray] [IntVar] (VarNode "b" mockNodeDat))

                it "Should throw an error if variable re-declarations have different types" $
                  (extractError (ProgramNode
                                 [DeclarationNode
                                  (VarNode "a" mockNodeDat)
                                  IntVar
                                  Nothing
                                  mockNodeDat,
                                  DeclarationNode
                                  (VarNode "a" mockNodeDat)
                                  IntPointer
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch
                             [IntVar]
                             [IntPointer]
                             (DeclarationNode
                              (VarNode "a" mockNodeDat)
                              IntPointer
                              Nothing
                              mockNodeDat
                             )
                            )

                it "Should throw an error if attempting to assign a binary node of the wrong type" $
                  (extractError (ProgramNode
                                 [DeclarationNode
                                  (VarNode "b" mockNodeDat)
                                  IntArray
                                  (Just $ AssignmentNode
                                   (VarNode "b" mockNodeDat)
                                   (BinaryNode
                                    (ConstantNode 12 mockNodeDat)
                                    (ConstantNode 3 mockNodeDat)
                                    Plus
                                    mockNodeDat
                                   )
                                   Assignment
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch [IntArray] [IntVar] (VarNode "b" mockNodeDat))

                it "Should throw an error if attempting to assign a ternary node of the wrong type" $
                  (extractError (ProgramNode
                                 [DeclarationNode
                                  (VarNode "b" mockNodeDat)
                                  IntArray
                                  (Just $ AssignmentNode
                                   (VarNode "b" mockNodeDat)
                                   (TernaryNode
                                    (BinaryNode
                                     (ConstantNode 12 mockNodeDat)
                                     (ConstantNode 3 mockNodeDat)
                                     GreaterThan
                                     mockNodeDat
                                    )
                                    (ConstantNode 1 mockNodeDat)
                                    (ConstantNode 2 mockNodeDat)
                                    mockNodeDat
                                   )
                                   Assignment
                                   mockNodeDat
                                  )
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch [IntArray] [IntVar] (VarNode "b" mockNodeDat))

                it "Should throw error if function declaration types don't match" $
                  (extractError (ProgramNode
                                 [FunctionNode
                                  IntVar
                                  "dog"
                                  []
                                  Nothing
                                  mockNodeDat,
                                  FunctionNode
                                  IntPointer
                                  "dog"
                                  []
                                  Nothing
                                  mockNodeDat
                                 ]
                                )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch
                             [IntVar]
                             [IntPointer]
                             (FunctionNode
                              IntPointer
                              "dog"
                              []
                              Nothing
                              mockNodeDat
                             )
                            )
