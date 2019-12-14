
module GeneratorSpec (generatorTest) where


import Data.Either
import Test.Hspec

import Generator
import Tokens
import AST
import Error
import VarTypes


generatorTest :: IO ()
generatorTest = hspec $ do
        describe "Build output string from AST" $ do
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
