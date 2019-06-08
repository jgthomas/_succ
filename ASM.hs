
module ASM where


import Tokens     (Operator(..))
import ASM_Tokens (Jump(..))


-- Functions

functionName :: String -> String
functionName f = ".globl "
                 ++ f
                 ++ "\n"
                 ++ f
                 ++ ":\n"
                 ++ saveBasePointer


returnStatement :: String
returnStatement = restoreBasePointer ++ "ret\n"


saveBasePointer :: String
saveBasePointer = "pushq %rbp\n"
                  ++ "movq %rsp, %rbp\n"


restoreBasePointer :: String
restoreBasePointer = "movq %rbp, %rsp\n"
                     ++ "popq %rbp\n"


-- Local variables

loadValue :: Int -> String
loadValue n = "movq $" ++ show n ++ ", %rax\n"


varOnStack :: Int -> String
varOnStack n = "movq %rax, " ++ show n ++ "(%rbp)\n"


varOffStack :: Int -> String
varOffStack n = "movq " ++ show n ++ "(%rbp), %rax\n"


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        "movq %rbp, %rsp\n"
        ++ "subq $" ++ show offset ++ ", %rsp\n"


-- Operators

unary :: Operator -> String
unary o
   | o == Minus         = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"


binary :: String -> String -> Operator -> String
binary val1 val2 o
   | o == Plus               = loadValues val1 val2 ++ "addq %rcx, %rax\n"
   | o == Multiply           = loadValues val1 val2 ++ "imul %rcx, %rax\n"
   | o == Minus              = loadValues val2 val1 ++ "subq %rcx, %rax\n"
   | o == Divide             = loadValues val2 val1 ++ "cqto\n" ++ "idivq %rcx\n"
   | o == Modulo             = loadValues val2 val1 ++ moduloValues
   | o == Equal              = comparison val1 val2 ++ "sete %al\n"
   | o == NotEqual           = comparison val1 val2 ++ "setne %al\n"
   | o == GreaterThan        = comparison val1 val2 ++ "setg %al\n"
   | o == LessThan           = comparison val1 val2 ++ "setl %al\n"
   | o == GreaterThanOrEqual = comparison val1 val2 ++ "setge %al\n"
   | o == LessThanOrEqual    = comparison val1 val2 ++ "setle %al\n"


logicalOR :: String -> String -> Int -> Int -> String
logicalOR val1 val2 nextLabel endLabel = val1
                       ++ "cmpq $0, %rax\n"
                       ++ emitJump JE nextLabel
                       ++ "movq $1, %rax\n"
                       ++ emitJump JMP endLabel
                       ++ emitLabel nextLabel
                       ++ val2
                       ++ "cmpq $0, %rax\n"
                       ++ "movq $0, %rax\n"
                       ++ "setne %al\n"
                       ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND val1 val2 nextLabel endLabel = val1
                       ++ "cmpq $0, %rax\n"
                       ++ emitJump JNE nextLabel
                       ++ emitJump JMP endLabel
                       ++ emitLabel nextLabel
                       ++ val2
                       ++ "cmpq $0, %rax\n"
                       ++ "movq $0, %rax\n"
                       ++ "setne %al\n"
                       ++ emitLabel endLabel


loadValues :: String -> String -> String
loadValues val1 val2 = val1
                    ++ "pushq %rax\n"
                    ++ val2
                    ++ "popq %rcx\n"


comparison :: String -> String -> String
comparison val1 val2 = loadValues val1 val2
                    ++ "cmpq %rax, %rcx\n"
                    ++ "movq $0, %rax\n"


moduloValues :: String
moduloValues = "cqto\n"
            ++ "idivq %rcx\n"
            ++ "movq %rdx, %rax\n"


testResult :: String
testResult = "cmpq $0, %rax\n"


-- Jumps and labels

emitJump :: Jump -> Int -> String
emitJump j n
        | j == JMP  = "jmp _label_" ++ show n ++ "\n"
        | j == JE   = "je _label_" ++ show n ++ "\n"
        | j == JNE  = "jne _label_" ++ show n ++ "\n"
        | otherwise = error "Unrecognised type of jump"


emitLabel :: Int -> String
emitLabel n = "_label_" ++ show n ++ ":\n"


-- Function calls and registers

makeFunctionCall :: String -> String
makeFunctionCall funcName = "call " ++ funcName ++ "\n"


putInRegister :: String -> String
putInRegister reg = "movq %rax, " ++ reg ++ "\n"


getFromRegister :: String -> String
getFromRegister reg = "movq " ++ reg ++ ", %rax\n"


selectRegister :: Int -> String
selectRegister callConvSeq
        | callConvSeq == 0 = "%rdi"
        | callConvSeq == 1 = "%rsi"
        | callConvSeq == 2 = "%rdx"
        | callConvSeq == 3 = "%rcx"
        | callConvSeq == 4 = "%r8"
        | callConvSeq == 5 = "%r9"


saveCallerRegisters :: String
saveCallerRegisters =
        "pushq %rdi\n"
        ++ "pushq %rsi\n"
        ++ "pushq %rdx\n"
        ++ "pushq %rcx\n"
        ++ "pushq %r8\n"
        ++ "pushq %r9\n"


restoreCallerRegisters :: String
restoreCallerRegisters =
        "popq %r9\n"
        ++ "popq %r8\n"
        ++ "popq %rcx\n"
        ++ "popq %rdx\n"
        ++ "popq %rsi\n"
        ++ "popq %rdi\n"


-- Global variables

initializedGlobal :: String -> String -> String
initializedGlobal label val =
        ".globl " ++ label ++ "\n"
        ++ ".data\n"
        ++ ".align 4\n"
        ++ label ++ ":\n"
        ++ ".long " ++ val ++ "\n"
        ++ ".text\n"


uninitializedGlobal :: String -> String
uninitializedGlobal label =
        ".globl " ++ label ++ "\n"
        ++ ".bss\n"
        ++ ".align 4\n"
        ++ label ++ ":\n"
        ++ ".text\n"


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        "movq " ++ label ++ "(%rip), %rax\n"


storeGlobal :: String -> String
storeGlobal label =
        "movq %rax, " ++ label ++ "(%rip)\n"


-- Pointers

varAddressLoad :: Int -> String
varAddressLoad offset =
        "leaq " ++ show offset ++ "(%rbp), %rax\n"


varAddressStore :: Int -> String
varAddressStore offset =
        "movq %rax, " ++ show offset ++ "(%rbp)\n"


dereferenceLoad :: Int -> String
dereferenceLoad offset =
        "movq " ++ show offset ++ "(%rbp), %rcx\n"
        ++ "movq (%rcx), %rax\n"


dereferenceStore :: Int -> String
dereferenceStore offset =
        "movq " ++ show offset ++ "(%rbp), %rcx\n"
        ++ "movq %rax, (%rcx)\n"


-- General

noOutput :: String
noOutput = ""

