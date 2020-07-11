
module CheckerTest.CheckerTestSpec (checkerTest) where


import Test.Hspec

import CheckerTest.TestUtility (extractError, extractTree)
import TestUtility             (mockNodeDat)
import Types.AST
import Types.Error
import Types.Type


checkerTest :: IO ()
checkerTest = hspec $ do
        describe "Check abstract syntax tree for errors" $ do

                it "Should return a valid tree unchanged" $
                  (extractTree $ ProgramNode
                                [FunctionNode
                                 IntVar
                                 "main"
                                 []
                                 (Just $ CompoundStmtNode
                                  [ReturnNode
                                   (ConstantNode 2 mockNodeDat)
                                   mockNodeDat
                                  ]
                                  mockNodeDat
                                 )
                                 mockNodeDat
                                ]
                  )
                  `shouldBe`
                   ProgramNode [FunctionNode
                                IntVar
                                "main"
                                []
                                (Just $ CompoundStmtNode
                                 [ReturnNode
                                  (ConstantNode 2 mockNodeDat)
                                  mockNodeDat
                                 ]
                                 mockNodeDat
                                )
                                mockNodeDat
                               ]


                it "Should throw error if returning an undeclared variable" $
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
