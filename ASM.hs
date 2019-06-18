
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


-- Functions

functionName :: String -> String
functionName funcName =
        declareGlobl funcName
        ++ globlLabel funcName
        ++ saveBasePointer
        ++ saveResisters allScratch


returnStatement :: String
returnStatement =
        restoreRegisters allScratch
        ++ restoreBasePointer
        ++ returnControl


saveBasePointer :: String
saveBasePointer = push basePointer
                  ++ move stackPointer basePointer


restoreBasePointer :: String
restoreBasePointer = move basePointer stackPointer
                     ++ pop basePointer


-- Local variables

loadValue :: Int -> String
loadValue n = move (literalValue n) result


varOnStack :: Int -> String
varOnStack offset = move result (fromBasePointer offset)


varOffStack :: Int -> String
varOffStack offset = move (fromBasePointer offset) result


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move basePointer stackPointer
        ++ sub (literalValue offset) stackPointer


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
        ++ move (literalValue 1) result
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move (literalValue 0) result
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
        ++ move (literalValue 0) result
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
        ++ move "%rdx" result
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
        ++ comp result scratch
        ++ move (literalValue 0) result


testResult :: String
testResult = comp (literalValue 0) result


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
putInRegister reg = move result reg


getFromRegister :: String -> String
getFromRegister reg = move reg result


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
        declareGlobl label
        ++ ".data\n"
        ++ ".align 4\n"
        ++ globlLabel label
        ++ ".long " ++ val ++ "\n"
        ++ ".text\n"


uninitializedGlobal :: String -> String
uninitializedGlobal label =
        declareGlobl label
        ++ ".bss\n"
        ++ ".align 4\n"
        ++ globlLabel label
        ++ ".text\n"


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        move (fromInstructionPointer label) result


storeGlobal :: String -> String
storeGlobal label =
        move result (fromInstructionPointer label)


-- Pointers

derefLoadParam :: Int -> String
derefLoadParam reg =
        move (valueFromAddressIn . selectRegister $ reg) result


derefStoreParam :: Int -> String
derefStoreParam reg =
        move result (addressIn . selectRegister $ reg)


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) result


varAddressStore :: Int -> String
varAddressStore offset = move result (fromBasePointer offset)


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (valueFromAddressIn scratch) result


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (fromBasePointer offset) scratch
        ++ move result (addressIn scratch)


-- General

noOutput :: String
noOutput = ""

literalValue :: Int -> String
literalValue n = "$" ++ show n

fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) basePointer

fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab instrPointer

relAddress :: String -> String -> String
relAddress offset base = offset ++ "(" ++ base ++ ")"

addressIn :: String -> String
addressIn s = indirectAddressing s

valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s

indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


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

returnControl :: String
returnControl = "ret\n"

declareGlobl :: String -> String
declareGlobl name = ".globl " ++ name ++ "\n"

globlLabel :: String -> String
globlLabel name = name ++ ":\n"


-- Registers

result  = "%rax"
scratch = "%r12"

basePointer = "%rbp"
instrPointer = "%rip"
stackPointer = "%rsp"

allScratch = [scratch]
params = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"]
