
module GeneratorTest.GeneratorTestSpec (generatorTest) where


import Test.Hspec

import GeneratorTest.TestUtility (extractAssembly)
import TestUtility               (mockNodeDat)
import Types.AST
import Types.Operator
import Types.Type


generatorTest :: IO ()
generatorTest = hspec $ do
        describe "Generate assembly code from an abstract syntax tree" $ do

                it "Should output assembly for a constant" $
                  (extractAssembly (ConstantNode 2 mockNodeDat))
                  `shouldBe`
                  "2"

                it "Should output assembly for a null expression" $ do
                  (extractAssembly (NullExprNode mockNodeDat))
                  `shouldBe`
                  ""

                it "Should output assembly for a single main function" $
                  (extractAssembly (ProgramNode
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
                  )
                  `shouldBe`
                  unlines ["init:",
                           "jmp init_done",
                           ".globl main",
                           "main:",
                           "jmp init",
                           "init_done:",
                           "pushq %rbp",
                           "movq %rsp, %rbp",
                           "pushq %r12",
                           "movq $2, %rax",
                           "popq %r12",
                           "movq %rbp, %rsp",
                           "popq %rbp",
                           "ret"
                          ]


                it "Should output assembly for main function with a unary operator" $
                  (extractAssembly (ProgramNode
                                    [FunctionNode
                                     IntVar
                                     "main"
                                     []
                                     (Just $ CompoundStmtNode
                                      [ReturnNode
                                       (UnaryNode
                                        (ConstantNode 2 mockNodeDat)
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
                  unlines [
                           "init:",
                           "jmp init_done",
                           ".globl main",
                           "main:",
                           "jmp init",
                           "init_done:",
                           "pushq %rbp",
                           "movq %rsp, %rbp",
                           "pushq %r12",
                           "movq $2, %rax",
                           "neg %rax",
                           "popq %r12",
                           "movq %rbp, %rsp",
                           "popq %rbp",
                           "ret"
                          ]
