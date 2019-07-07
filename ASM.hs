
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
import ASM_Tokens (Jump(..), Section(..), Set(..))


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
   | o == Minus         = makeNegative result
   | o == BitwiseCompl  = invertBits result
   | o == LogicNegation = comp (literalValue 0) result
                          ++ move (literalValue 0) result
                          ++ setBitIf Equ


binary :: String -> String -> Operator -> String
binary load1 load2 o
   | o == Plus               = computeAdd load1 load2
   | o == Minus              = computeSub load1 load2
   | o == Multiply           = computeMul load1 load2
   | o == Divide             = computeDiv load1 load2
   | o == Modulo             = computeMod load1 load2
   | o == Equal              = comparison load1 load2 ++ setBitIf Equ
   | o == NotEqual           = comparison load1 load2 ++ setBitIf NEqu
   | o == GreaterThan        = comparison load1 load2 ++ setBitIf GThan
   | o == LessThan           = comparison load1 load2 ++ setBitIf LThan
   | o == GreaterThanOrEqual = comparison load1 load2 ++ setBitIf GThanE
   | o == LessThanOrEqual    = comparison load1 load2 ++ setBitIf LThanE


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
        ++ setBitIf NEqu
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
        ++ setBitIf NEqu
        ++ emitLabel endLabel


computeAdd :: String -> String -> String
computeAdd load1 load2 =
        loadValues load1 load2
        ++ add scratch result


computeMod :: String -> String -> String
computeMod load1 load2 =
        push regArg3
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ move regModResult result
        ++ pop regArg3


computeDiv :: String -> String -> String
computeDiv load1 load2 =
        push regArg3
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ pop regArg3


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


-- Function calls and registers

makeFunctionCall :: String -> String
makeFunctionCall funcName = call funcName


putInRegister :: String -> String
putInRegister reg = move result reg


getFromRegister :: String -> String
getFromRegister reg = move reg result


selectRegister :: Int -> String
selectRegister callConvSeq
        | callConvSeq == 0 = regArg1
        | callConvSeq == 1 = regArg2
        | callConvSeq == 2 = regArg3
        | callConvSeq == 3 = regArg4
        | callConvSeq == 4 = regArg5
        | callConvSeq == 5 = regArg6


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
        ++ section DATA
        ++ align
        ++ globlLabel label
        ++ asLong val
        ++ section TEXT


uninitializedGlobal :: String -> String
uninitializedGlobal label =
        declareGlobl label
        ++ section BSS
        ++ align
        ++ globlLabel label
        ++ section TEXT


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


derefLoadGlobal :: String -> String
derefLoadGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (valueFromAddressIn scratch) result


derefStoreGlobal :: String -> String
derefStoreGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move result (addressIn scratch)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) result


varAddressStoreGlobal :: String -> String
varAddressStoreGlobal label = move result (fromInstructionPointer label)


-- Addressing

fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) basePointer

fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab instrPointer

addressIn :: String -> String
addressIn s = indirectAddressing s

valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s

relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base

indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


-- Other

literalValue :: Int -> String
literalValue n = "$" ++ show n

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
idivq target = signExtendRaxRdx
               ++ "idivq " ++ target ++ "\n"

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

makeNegative :: String -> String
makeNegative s = "neg " ++ s ++ "\n"

invertBits :: String -> String
invertBits s = "not " ++ s ++ "\n"

returnControl :: String
returnControl = "ret\n"

setBitIf :: Set -> String
setBitIf set
        | set == Equ    = "sete " ++ bit ++ "\n"
        | set == NEqu   = "setne " ++ bit ++ "\n"
        | set == GThan  = "setg " ++ bit ++ "\n"
        | set == GThanE = "setge " ++ bit ++ "\n"
        | set == LThan  = "setl " ++ bit ++ "\n"
        | set == LThanE = "setle " ++ bit ++ "\n"
        where bit = "%al"

signExtendRaxRdx :: String
signExtendRaxRdx = "cqto\n"

emitJump :: Jump -> Int -> String
emitJump j n
        | j == JMP  = "jmp _label_" ++ show n ++ "\n"
        | j == JE   = "je _label_" ++ show n ++ "\n"
        | j == JNE  = "jne _label_" ++ show n ++ "\n"
        | otherwise = error "Unrecognised type of jump"


-- Directives

declareGlobl :: String -> String
declareGlobl name = ".globl " ++ name ++ "\n"

globlLabel :: String -> String
globlLabel name = name ++ ":\n"

section :: Section -> String
section sect
        | sect == TEXT = ".text\n"
        | sect == DATA = ".data\n"
        | sect == BSS  = ".bss\n"

align :: String
align = ".align 4\n"

asLong :: String -> String
asLong l = ".long " ++ l ++ "\n"

emitLabel :: Int -> String
emitLabel n = "_label_" ++ show n ++ ":\n"


-- Registers

result  = "%rax"
scratch = "%r12"

basePointer  = "%rbp"
instrPointer = "%rip"
stackPointer = "%rsp"

regArg1 = "%rdi"
regArg2 = "%rsi"
regArg3 = "%rdx"
regArg4 = "%rcx"
regArg5 = "%r8"
regArg6 = "%r9"

regModResult = "%rdx"

allScratch = [scratch]
params = [regArg1,regArg2,regArg3,regArg4,regArg5,regArg6]
