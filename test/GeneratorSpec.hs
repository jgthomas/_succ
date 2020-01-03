
module GeneratorSpec (generatorTest) where


import Data.Either
import Test.Hspec

import AST
import Error
import Generator
import Type


generatorTest :: IO ()
generatorTest = hspec $ do
        describe "Build output string from AST" $ do
                it "Should output ASM for global constant" $
                  fromRight "FAIL" (generate (ConstantNode 2))
                  `shouldBe`
                  "2"

                it "Should make asm for valid AST" $
                  fromRight "FAIL" (generate
                                    (ProgramNode
                                     [FunctionNode
                                      IntVar
                                      "main"
                                      []
                                      (Just
                                       [ReturnNode
                                        (ConstantNode 2)
                                       ]
                                      )
                                     ]
                                    )
                                   )
                  `shouldBe`
                  unlines [".globl main",
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
                           "ret",
                           "init:",
                           "jmp init_done"
                          ]
