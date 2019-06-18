
module ASM (functionName,
            returnStatement,
            loadValue,
            varOnStack,
            varOffStack,
            adjustStackPointer,
            unary,
            binary,
            logicalOR,
            logicalAND,
            testResult,
            emitJump,
            emitLabel,
            makeFunctionCall,
            putInRegister,
            getFromRegister,
            selectRegister,
            saveCallerRegisters,
            restoreCallerRegisters,
            initializedGlobal,
            uninitializedGlobal,
            loadGlobal,
            storeGlobal,
            varAddressLoad,
            varAddressStore,
            derefLoadLocal,
            derefStoreLocal,
            derefLoadParam,
            derefStoreParam,
            noOutput) where


import Tokens     (Operator(..))
import ASM_Tokens (Jump(..))


-- Registers

result  = "%rax"
scratch = "%r12"

allScratch = [scratch]
params = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"]


-- Functions

functionName :: String -> String
functionName f =
        ".globl "
        ++ f
        ++ "\n"
        ++ f
        ++ ":\n"
        ++ saveBasePointer
        ++ saveResisters allScratch


returnStatement :: String
returnStatement =
        restoreRegisters allScratch
        ++ restoreBasePointer
        ++ "ret\n"


saveBasePointer :: String
saveBasePointer = push "%rbp"
                  ++ move "%rsp" "%rbp"


restoreBasePointer :: String
restoreBasePointer = move "%rbp" "%rsp"
                     ++ pop "%rbp"


-- Local variables

loadValue :: Int -> String
loadValue n = move (literalValue n) "%rax"


varOnStack :: Int -> String
varOnStack offset = move "%rax" (relAddress offset)


varOffStack :: Int -> String
varOffStack offset = move (relAddress offset) "%rax"


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move "%rbp" "%rsp"
        ++ sub (literalValue offset) "%rsp"


literalValue :: Int -> String
literalValue n = "$" ++ show n


-- Operators

unary :: Operator -> String
unary o
   | o == Minus         = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"


binary :: String -> String -> Operator -> String
binary val1 val2 o
   | o == Plus               = computeAdd val1 val2
   | o == Minus              = computeSub val1 val2
   | o == Multiply           = computeMul val1 val2
   | o == Divide             = computeDiv val1 val2
   | o == Modulo             = computeMod val1 val2
   | o == Equal              = comparison val1 val2 ++ "sete %al\n"
   | o == NotEqual           = comparison val1 val2 ++ "setne %al\n"
   | o == GreaterThan        = comparison val1 val2 ++ "setg %al\n"
   | o == LessThan           = comparison val1 val2 ++ "setl %al\n"
   | o == GreaterThanOrEqual = comparison val1 val2 ++ "setge %al\n"
   | o == LessThanOrEqual    = comparison val1 val2 ++ "setle %al\n"


logicalOR :: String -> String -> Int -> Int -> String
logicalOR load1 load2 nextLabel endLabel =
        load1
        ++ testResult
        ++ emitJump JE nextLabel
        ++ move "$1" "%rax"
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move "$0" "%rax"
        ++ "setne %al\n"
        ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND load1 load2 nextLabel endLabel =
        load1
        ++ testResult
        ++ emitJump JNE nextLabel
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move "$0" "%rax"
        ++ "setne %al\n"
        ++ emitLabel endLabel


computeAdd :: String -> String -> String
computeAdd load1 load2 =
        loadValues load1 load2
        ++ add scratch result


computeMod :: String -> String -> String
computeMod load1 load2 =
        push "%rdx"
        ++ loadValues load2 load1
        ++ "cqto\n"
        ++ idivq scratch
        ++ move "%rdx" "%rax"
        ++ pop "%rdx"


computeDiv :: String -> String -> String
computeDiv load1 load2 =
        push "%rdx"
        ++ loadValues load2 load1
        ++ "cqto\n"
        ++ idivq scratch
        ++ pop "%rdx"


computeMul :: String -> String -> String
computeMul load1 load2 =
        loadValues load1 load2
        ++ imul scratch result


computeSub :: String -> String -> String
computeSub load1 load2 =
        loadValues load2 load1
        ++ sub scratch result


loadValues :: String -> String -> String
loadValues load1 load2 =
        load1
        ++ push result
        ++ load2
        ++ pop scratch


comparison :: String -> String -> String
comparison load1 load2 =
        loadValues load1 load2
        ++ comp "%rax" scratch
        ++ move "$0" "%rax"


testResult :: String
testResult = comp "$0" "%rax"


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
makeFunctionCall funcName = call funcName


putInRegister :: String -> String
putInRegister reg = move "%rax" reg


getFromRegister :: String -> String
getFromRegister reg = move reg "%rax"


selectRegister :: Int -> String
selectRegister callConvSeq
        | callConvSeq == 0 = "%rdi"
        | callConvSeq == 1 = "%rsi"
        | callConvSeq == 2 = "%rdx"
        | callConvSeq == 3 = "%rcx"
        | callConvSeq == 4 = "%r8"
        | callConvSeq == 5 = "%r9"


saveResisters :: [String] -> String
saveResisters regs = concat . map push $ regs


restoreRegisters :: [String] -> String
restoreRegisters regs = concat . map pop . reverse $ regs


saveCallerRegisters :: String
saveCallerRegisters = saveResisters params


restoreCallerRegisters :: String
restoreCallerRegisters = restoreRegisters params


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
loadGlobal label = move (label ++ "(%rip)") "%rax"


storeGlobal :: String -> String
storeGlobal label = move "%rax" (label ++ "(%rip)")


-- Pointers

derefLoadParam :: Int -> String
derefLoadParam reg =
        move ("(" ++ selectRegister reg ++ ")") "%rax"


derefStoreParam :: Int -> String
derefStoreParam reg =
        move "%rax" ("(" ++ selectRegister reg ++ ")")


relAddress :: Int -> String
relAddress offset = show offset ++ "(%rbp)"


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (relAddress offset) "%rax"


varAddressStore :: Int -> String
varAddressStore offset = move "%rax" (relAddress offset)


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (relAddress offset) scratch
        ++ move ("(" ++ scratch ++ ")") "%rax"


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (relAddress offset) scratch
        ++ move "%rax" ("(" ++ scratch ++ ")")


-- General

noOutput :: String
noOutput = ""


-- Instructions

add :: String -> String -> String
add a b = "addq " ++ a ++ ", " ++ b ++ "\n"

imul :: String -> String -> String
imul a b = "imul " ++ a ++ ", " ++ b ++ "\n"

sub :: String -> String -> String
sub a b = "subq " ++ a ++ ", " ++ b ++ "\n"

idivq :: String -> String
idivq target = "idivq " ++ target ++ "\n"

push :: String -> String
push s = "pushq " ++ s ++ "\n"

pop :: String -> String
pop s = "popq " ++ s ++ "\n"

call :: String -> String
call f = "call " ++ f ++ "\n"

move :: String -> String -> String
move s d = "movq " ++ s ++ ", " ++ d ++ "\n"

loadAddOf :: String -> String -> String
loadAddOf s d = "leaq " ++ s ++ ", " ++ d ++ "\n"

comp :: String -> String -> String
comp a b = "cmpq " ++ a ++ ", " ++ b ++ "\n"

