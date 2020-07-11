
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

                it "Should output the literal integer in assembly for a constant" $
                  (extractAssembly (ConstantNode 2 mockNodeDat))
                  `shouldBe`
                  "2"

                it "Should output no assembly code for a null expression" $
                  (extractAssembly (NullExprNode mockNodeDat))
                  `shouldBe`
                  ""

                it "Should output no assembly code for a function declaration" $
                  (extractAssembly (FunctionNode
                                    IntVar
                                    "dog"
                                    []
                                    Nothing
                                    mockNodeDat
                                   )
                  )
                  `shouldBe`
                  ""

                it "Should output no assembly code for a function declaration with parameters" $
                  (extractAssembly (FunctionNode
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
                                    mockNodeDat
                                   )
                  )
                  `shouldBe`
                  ""

                it "Should output assembly for a main function" $
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
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret"
                          ]

                it "Should output assembly for a main function with no return statement" $
                  (extractAssembly (ProgramNode
                                    [FunctionNode
                                     IntVar
                                     "main"
                                     []
                                     (Just $ CompoundStmtNode [] mockNodeDat)
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
                          "movq $0, %rax",
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

                it "Should output assembly for program with two functions" $
                  (extractAssembly (ProgramNode
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
                                   )
                  `shouldBe`
                  unlines [
                          "init:",
                          "jmp init_done",
                          ".globl dog",
                          "dog:",
                          "pushq %rbp",
                          "movq %rsp, %rbp",
                          "pushq %r12",
                          "movq $2, %rax",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret",
                          ".globl main",
                          "main:",
                          "jmp init",
                          "init_done:",
                          "pushq %rbp",
                          "movq %rsp, %rbp",
                          "pushq %r12",
                          "pushq %rdi",
                          "pushq %rsi",
                          "pushq %rdx",
                          "pushq %rcx",
                          "pushq %r8",
                          "pushq %r9",
                          "call dog",
                          "popq %r9",
                          "popq %r8",
                          "popq %rcx",
                          "popq %rdx",
                          "popq %rsi",
                          "popq %rdi",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret"
                          ]

                it "Should place any uninitialised global variables in the bss section" $
                  (extractAssembly (ProgramNode
                                    [DeclarationNode
                                     (VarNode "dog" mockNodeDat)
                                     IntVar
                                     Nothing
                                     mockNodeDat,
                                     FunctionNode
                                     IntVar
                                     "main"
                                     []
                                     (Just $ CompoundStmtNode
                                      [ReturnNode
                                       (VarNode "dog" mockNodeDat)
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
                          ".globl _dog1",
                          ".bss",
                          ".align 4",
                          "_dog1:",
                          ".text",
                          ".globl main",
                          "main:",
                          "jmp init",
                          "init_done:",
                          "pushq %rbp",
                          "movq %rsp, %rbp",
                          "pushq %r12",
                          "movq _dog1+0(%rip), %rax",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret"
                          ]

                it "Should place initialised global variables in the data section" $
                  (extractAssembly (ProgramNode
                                    [DeclarationNode
                                     (VarNode "cat" mockNodeDat)
                                     IntVar
                                     (Just $ AssignmentNode
                                      (VarNode "cat" mockNodeDat)
                                      (ConstantNode 3 mockNodeDat)
                                      Assignment
                                      mockNodeDat
                                     )
                                     mockNodeDat,
                                     FunctionNode
                                     IntVar
                                     "main"
                                     []
                                     (Just $ CompoundStmtNode
                                      [ReturnNode
                                       (VarNode "cat" mockNodeDat)
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
                          ".globl _cat1",
                          ".data",
                          ".align 4",
                          "_cat1:",
                          ".long 3",
                          ".text",
                          ".globl main",
                          "main:",
                          "jmp init",
                          "init_done:",
                          "pushq %rbp",
                          "movq %rsp, %rbp",
                          "pushq %r12",
                          "movq _cat1+0(%rip), %rax",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret"
                          ]

                it "Should output assembly for program with simple if statement" $
                  (extractAssembly (ProgramNode
                                    [FunctionNode
                                     IntVar
                                     "main"
                                     []
                                     (Just $ CompoundStmtNode
                                      [IfNode
                                       (BinaryNode
                                        (ConstantNode 2 mockNodeDat)
                                        (ConstantNode 2 mockNodeDat)
                                        Equal
                                        mockNodeDat
                                       )
                                       (ReturnNode
                                        (ConstantNode 3 mockNodeDat)
                                        mockNodeDat
                                       )
                                       Nothing
                                       mockNodeDat,
                                       (ReturnNode
                                        (ConstantNode 2 mockNodeDat)
                                        mockNodeDat
                                       )
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
                          "pushq %rax",
                          "movq $2, %rax",
                          "popq %r12",
                          "cmpq %rax, %r12",
                          "movq $0, %rax",
                          "sete %al",
                          "cmpq $0, %rax",
                          "je _label_3",
                          "movq $3, %rax",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret",
                          "_label_3:",
                          "movq $2, %rax",
                          "popq %r12",
                          "movq %rbp, %rsp",
                          "popq %rbp",
                          "ret"
                          ]
