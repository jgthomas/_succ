
module GeneratorTest.GeneratorTestSpec (generatorTest) where


import Test.Hspec

import GeneratorTest.TestUtility (extractAssembly)
import TestUtility               (mockNodeDat)
import Types.AST


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


--
--generatorTest :: IO ()
--generatorTest = hspec $ do
--        describe "Build output string from AST" $ do
--                it "Should output ASM for global constant" $
--                  fromRight "FAIL" (generate (ConstantNode 2))
--                  `shouldBe`
--                  "2"
--
--                it "Should output ASM for null expression" $
--                  fromRight "FAIL" (generate (NullExprNode))
--                  `shouldBe`
--                  ""
--
--                it "Should output ASM for main function" $
--                  fromRight "FAIL" (generate
--                                    (ProgramNode
--                                     [FunctionNode
--                                      IntVar
--                                      "main"
--                                      []
--                                      (Just [
--                                       ReturnNode
--                                        (ConstantNode 2)
--                                      ])
--                                      TestUtility.mkNodeDat
--                                     ]
--                                    )
--                                   )
--                  `shouldBe`
--                  unlines [".globl main",
--                           "main:",
--                           "jmp init",
--                           "init_done:",
--                           "pushq %rbp",
--                           "movq %rsp, %rbp",
--                           "pushq %r12",
--                           "movq $2, %rax",
--                           "popq %r12",
--                           "movq %rbp, %rsp",
--                           "popq %rbp",
--                           "ret",
--                           "init:",
--                           "jmp init_done"
--                          ]
--
--                it "Should output ASM for main function with unary operator" $
--                  fromRight "FAIL" (generate
--                                    (ProgramNode
--                                     [FunctionNode
--                                      IntVar
--                                      "main"
--                                      []
--                                      (Just [
--                                       ReturnNode
--                                        (UnaryNode
--                                         (ConstantNode 2)
--                                         (Unary Negate)
--                                        )
--                                      ])
--                                      TestUtility.mkNodeDat
--                                     ]
--                                    )
--                                   )
--                  `shouldBe`
--                  unlines [".globl main",
--                           "main:",
--                           "jmp init",
--                           "init_done:",
--                           "pushq %rbp",
--                           "movq %rsp, %rbp",
--                           "pushq %r12",
--                           "movq $2, %rax",
--                           "neg %rax",
--                           "popq %r12",
--                           "movq %rbp, %rsp",
--                           "popq %rbp",
--                           "ret",
--                           "init:",
--                           "jmp init_done"
--                          ]
