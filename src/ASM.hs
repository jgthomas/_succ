
module ASM
        (function,
         mainNoReturn,
         returnStatement,
         loadValue,
         varOnStack,
         varOffStack,
         adjustStackPointer,
         unary,
         binary,
         ternary,
         functionCall,
         while,
         doWhile,
         ifOnly,
         ifElse,
         forLoop,
         emitJump,
         emitLabel,
         putInRegister,
         getFromRegister,
         selectRegister,
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
         derefLoadGlobal,
         derefStoreGlobal,
         varAddressLoadGlobal,
         varAddressStoreGlobal,
         outputInit,
         allUninitialized,
         noOutput
        ) where


import GenTokens (Jump (..), Register (..), Section (..), Set (..))
import Operator  (BinaryOp (..), UnaryOp (..))


-- Functions

function :: String -> String -> String
function name stmts =
        functionName name
        ++ stmts


mainNoReturn :: String -> String -> String
mainNoReturn name stmts =
        function name stmts
        ++ loadValue 0
        ++ returnStatement


functionName :: String -> String
functionName funcName =
        declareGlobl funcName
        ++ globlLabel funcName
        ++ runInit funcName
        ++ saveBasePointer
        ++ saveRegisters allScratch


returnStatement :: String
returnStatement =
        restoreRegisters allScratch
        ++ restoreBasePointer
        ++ returnControl


saveBasePointer :: String
saveBasePointer = push (reg RBP)
                  ++ move (reg RSP) (reg RBP)


restoreBasePointer :: String
restoreBasePointer = move (reg RBP) (reg RSP)
                     ++ pop (reg RBP)


functionCall :: String -> String -> String
functionCall name args =
        saveCallerRegisters
        ++ args
        ++ makeFunctionCall name
        ++ restoreCallerRegisters


-- Local variables

loadValue :: Int -> String
loadValue n = move (literalValue n) (reg RAX)


varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


varOffStack :: Int -> String
varOffStack offset = move (fromBasePointer offset) (reg RAX)


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literalValue offset) (reg RSP)


-- Statements

while :: String -> String -> Int -> Int -> String
while test body loopLab testLab =
        emitLabel loopLab
        ++ test
        ++ testResult
        ++ emitJump JE testLab
        ++ body
        ++ emitJump JMP loopLab
        ++ emitLabel testLab


doWhile :: String -> String -> Int -> Int -> Int -> String
doWhile body test loopLab contLab testLab =
        emitLabel loopLab
        ++ body
        ++ emitLabel contLab
        ++ test
        ++ testResult
        ++ emitJump JE testLab
        ++ emitJump JMP loopLab
        ++ emitLabel testLab


forLoop :: String
        -> String
        -> String
        -> String
        -> Int
        -> Int
        -> Int
        -> String
forLoop inits test iter body trueLab falseLab contLab =
        inits
        ++ emitLabel trueLab
        ++ test
        ++ testResult
        ++ emitJump JE falseLab
        ++ body
        ++ emitLabel contLab
        ++ iter
        ++ emitJump JMP trueLab
        ++ emitLabel falseLab



ifOnly :: String -> String -> Int -> String
ifOnly test action testLab =
        ifStart test action testLab
        ++ emitLabel testLab


ifElse :: String -> String -> Int -> String -> Int -> String
ifElse test action testLab elseAction nextLab =
        ifStart test action testLab
        ++ emitJump JMP nextLab
        ++ emitLabel testLab
        ++ elseAction
        ++ emitLabel nextLab


ifStart :: String -> String -> Int -> String
ifStart test action testLab =
        test
        ++ testResult
        ++ emitJump JE testLab
        ++ action


-- Operators

ternary :: String -> String -> String -> Int -> Int -> String
ternary test true false trueLab falseLab =
        test
        ++ testResult
        ++ emitJump JE falseLab
        ++ true
        ++ emitJump JMP trueLab
        ++ emitLabel falseLab
        ++ false
        ++ emitLabel trueLab


unary :: UnaryOp -> String
unary unOp =
        case unOp of
             Negate      -> makeNegative (reg RAX)
             BitwiseComp -> invertBits (reg RAX)
             LogicalNeg  ->
                     comp (literalValue 0) (reg RAX)
                          ++ move (literalValue 0) (reg RAX)
                          ++ setBitIf Equ


binary :: String -> String -> BinaryOp -> Int -> Int -> String
binary load1 load2 binOp lab1 lab2 =
        case binOp of
             Plus        -> computeAdd load1 load2
             Minus       -> computeSub load1 load2
             Multiply    -> computeMul load1 load2
             Divide      -> computeDiv load1 load2
             Modulo      -> computeMod load1 load2
             Equal       -> comparison load1 load2 ++ setBitIf Equ
             NotEqual    -> comparison load1 load2 ++ setBitIf NEqu
             GreaterThan -> comparison load1 load2 ++ setBitIf GThan
             LessThan    -> comparison load1 load2 ++ setBitIf LThan
             GThanOrEqu  -> comparison load1 load2 ++ setBitIf GThanE
             LThanOrEqu  -> comparison load1 load2 ++ setBitIf LThanE
             LogicalOR   -> logicalOR load1 load2 lab1 lab2
             LogicalAND  -> logicalAND load1 load2 lab1 lab2
             Assignment  -> noOutput


logicalOR :: String -> String -> Int -> Int -> String
logicalOR load1 load2 nextLabel endLabel =
        load1
        ++ testResult
        ++ emitJump JE nextLabel
        ++ move (literalValue 1) (reg RAX)
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move (literalValue 0) (reg RAX)
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
        ++ move (literalValue 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


computeAdd :: String -> String -> String
computeAdd load1 load2 =
        loadValues load1 load2
        ++ add scratch (reg RAX)


computeMod :: String -> String -> String
computeMod load1 load2 =
        push (reg RDX)
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ move regModResult (reg RAX)
        ++ pop (reg RDX)


computeDiv :: String -> String -> String
computeDiv load1 load2 =
        push (reg RDX)
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ pop (reg RDX)


computeMul :: String -> String -> String
computeMul load1 load2 =
        loadValues load1 load2
        ++ imul scratch (reg RAX)


computeSub :: String -> String -> String
computeSub load1 load2 =
        loadValues load2 load1
        ++ sub scratch (reg RAX)


loadValues :: String -> String -> String
loadValues load1 load2 =
        load1
        ++ push (reg RAX)
        ++ load2
        ++ pop scratch


comparison :: String -> String -> String
comparison load1 load2 =
        loadValues load1 load2
        ++ comp (reg RAX) scratch
        ++ move (literalValue 0) (reg RAX)


testResult :: String
testResult = comp (literalValue 0) (reg RAX)


-- Function calls and registers

makeFunctionCall :: String -> String
makeFunctionCall funcName = call funcName


putInRegister :: String -> String
putInRegister r = move (reg RAX) r


getFromRegister :: String -> String
getFromRegister r = move r (reg RAX)


selectRegister :: Int -> String
selectRegister n
        | n == 0 = reg RDI
        | n == 1 = reg RSI
        | n == 2 = reg RDX
        | n == 3 = reg RCX
        | n == 4 = reg R8
        | n == 5 = reg R9
        | otherwise = undefined


saveRegisters :: [Register] -> String
saveRegisters rs = concatMap (push . reg) rs


restoreRegisters :: [Register] -> String
restoreRegisters rs = concatMap pop . reverse . map reg $ rs


saveCallerRegisters :: String
saveCallerRegisters = saveRegisters params


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


allUninitialized :: [String] -> String
allUninitialized vars = concatMap uninitializedGlobal vars


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        move (fromInstructionPointer label) (reg RAX)


storeGlobal :: String -> String
storeGlobal label =
        move (reg RAX) (fromInstructionPointer label)


-- Pointers

derefLoadParam :: Int -> String
derefLoadParam r =
        move (valueFromAddressIn . selectRegister $ r) (reg RAX)


derefStoreParam :: Int -> String
derefStoreParam r =
        move (reg RAX) (addressIn . selectRegister $ r)


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressStore :: Int -> String
varAddressStore offset = move (reg RAX) (fromBasePointer offset)


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (reg RAX) (addressIn scratch)


derefLoadGlobal :: String -> String
derefLoadGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


derefStoreGlobal :: String -> String
derefStoreGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (reg RAX) (addressIn scratch)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) (reg RAX)


varAddressStoreGlobal :: String -> String
varAddressStoreGlobal label = move (reg RAX) (fromInstructionPointer label)


-- Addressing

fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)

fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)

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

outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"

runInit :: String -> String
runInit name = if name == "main"
               then "jmp init\n" ++ "init_done:\n"
               else noOutput

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
setBitIf set =
        case set of
             Equ    -> "sete " ++ bit ++ "\n"
             NEqu   -> "setne " ++ bit ++ "\n"
             GThan  -> "setg " ++ bit ++ "\n"
             GThanE -> "setge " ++ bit ++ "\n"
             LThan  -> "setl " ++ bit ++ "\n"
             LThanE -> "setle " ++ bit ++ "\n"
        where bit = "%al"

signExtendRaxRdx :: String
signExtendRaxRdx = "cqto\n"

emitJump :: Jump -> Int -> String
emitJump j n =
        case j of
             JMP -> "jmp _label_" ++ num ++ "\n"
             JE  -> "je _label_" ++ num ++ "\n"
             JNE -> "jne _label_" ++ num ++ "\n"
        where num = show n


-- Directives

declareGlobl :: String -> String
declareGlobl name = ".globl " ++ name ++ "\n"

globlLabel :: String -> String
globlLabel name = name ++ ":\n"

section :: Section -> String
section sect =
        case sect of
             TEXT -> ".text\n"
             DATA -> ".data\n"
             BSS  -> ".bss\n"

align :: String
align = ".align 4\n"

asLong :: String -> String
asLong l = ".long " ++ l ++ "\n"

emitLabel :: Int -> String
emitLabel n = "_label_" ++ show n ++ ":\n"


-- Registers

reg :: Register -> String
reg r = case r of
             RAX -> "%rax"
             RBP -> "%rbp"
             RIP -> "%rip"
             RSP -> "%rsp"
             RDI -> "%rdi"
             RSI -> "%rsi"
             RDX -> "%rdx"
             RCX -> "%rcx"
             R8  -> "%r8"
             R9  -> "%r9"
             R12 -> "%r12"

scratch :: String
scratch = reg R12

regModResult :: String
regModResult = reg RDX

allScratch :: [Register]
allScratch = [R12]

params :: [Register]
params = [RDI,
          RSI,
          RDX,
          RCX,
          R8,
          R9
         ]
