{-|
Module       : ASM
Description  : Builds strings of assembly code

Generates the strings of x86-64 assembly code.
-}
module ASM
        (function,
         mainNoReturn,
         returnValue,
         loadLiteral,
         unary,
         binary,
         ternary,
         functionCall,
         decNoAssign,
         assign,
         while,
         doWhile,
         ifOnly,
         ifElse,
         forLoop,
         setGotoPoint,
         passArgument,
         initializedGlobal,
         uninitializedGlobal,
         storeGlobal,
         varAddressStore,
         derefLoad,
         derefStore,
         addressOf,
         loadVariable,
         varAddressStoreGlobal,
         outputInit,
         allUninitialized,
         noOutput
        ) where


import Error    (CompilerError (GeneratorError, ImpossibleError),
                 GeneratorError (..))
import GenState (GenState, throwError)
import Operator (BinaryOp (..), UnaryOp (..))


-- | Output asm for a function
function :: String -> String -> GenState String
function name stmts = pure $ functionInit name ++ stmts


-- | Output asm for a main function with no explicit return value
mainNoReturn :: String -> String -> GenState String
mainNoReturn name stmts = pure $
        functionInit name
        ++ stmts
        ++ loadValue 0
        ++ returnStatement


functionInit :: String -> String
functionInit funcName =
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


-- | Output asm for a return value
returnValue :: String -> GenState String
returnValue rtn = pure $ rtn ++ returnStatement


saveBasePointer :: String
saveBasePointer = push (reg RBP)
                  ++ move (reg RSP) (reg RBP)


restoreBasePointer :: String
restoreBasePointer = move (reg RBP) (reg RSP)
                     ++ pop (reg RBP)


-- | Output asm for a function call
functionCall :: String -> String -> GenState String
functionCall name args = pure $
        saveCallerRegisters
        ++ args
        ++ makeFunctionCall name
        ++ restoreCallerRegisters


-- | Output asm for a declaration with no assignment
decNoAssign :: Int -> Int -> GenState String
decNoAssign off adj = pure $
        loadValue 0
        ++ declare off adj


-- | Output asm for an assignment
assign :: String -> Int -> Int -> GenState String
assign toAssign off adj = pure $
        toAssign
        ++ declare off adj


declare :: Int -> Int -> String
declare off adj =
        varOnStack off
        ++ adjustStackPointer adj


-- | Output asm for jump
setGotoPoint :: Int -> GenState String
setGotoPoint target = pure $ emitJump JMP target


-- | Load a variable value
loadVariable :: Maybe Int -> Maybe Int -> Maybe String -> GenState String
loadVariable (Just off) _ _ = pure $ varOffStack off
loadVariable _ (Just pos) _ = pure $ getFromRegister pos
loadVariable _ _ (Just lab) = pure $ loadGlobal lab
loadVariable _ _ _          = throwError ImpossibleError


-- | Load a literal value into return register
loadLiteral :: Int -> GenState String
loadLiteral n = pure . loadValue $ n


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


-- | Output asm for while loop
while :: String -> String -> Int -> Int -> GenState String
while test body loopLab testLab = pure $
        emitLabel loopLab
        ++ test
        ++ testResult
        ++ emitJump JE testLab
        ++ body
        ++ emitJump JMP loopLab
        ++ emitLabel testLab


-- | Output asm for do while loop
doWhile :: String -> String -> Int -> Int -> Int -> GenState String
doWhile body test loopLab contLab testLab = pure $
        emitLabel loopLab
        ++ body
        ++ emitLabel contLab
        ++ test
        ++ testResult
        ++ emitJump JE testLab
        ++ emitJump JMP loopLab
        ++ emitLabel testLab


-- | Output asm for a for loop
forLoop :: String
        -> String
        -> String
        -> String
        -> Int
        -> Int
        -> Int
        -> GenState String
forLoop inits test iter body trueLab falseLab contLab = pure $
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


-- | Output asm for a simple if statement
ifOnly :: String -> String -> Int -> GenState String
ifOnly test action testLab = pure $
        ifStart test action testLab
        ++ emitLabel testLab


-- | Output asm for an if statement with an else clause
ifElse :: String -> String -> Int -> String -> Int -> GenState String
ifElse test action testLab elseAction nextLab = pure $
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


-- | Output asm for the ternary operator
ternary :: String -> String -> String -> Int -> Int -> GenState String
ternary test true false trueLab falseLab = pure $
        test
        ++ testResult
        ++ emitJump JE falseLab
        ++ true
        ++ emitJump JMP trueLab
        ++ emitLabel falseLab
        ++ false
        ++ emitLabel trueLab


-- | Output asm for unary operators
unary :: String -> UnaryOp -> GenState String
unary toLoad unOp = pure $ toLoad ++ unaryOp unOp


unaryOp :: UnaryOp -> String
unaryOp unOp =
        case unOp of
             Negate      -> makeNegative (reg RAX)
             BitwiseComp -> invertBits (reg RAX)
             LogicalNeg  ->
                     comp (literalValue 0) (reg RAX)
                          ++ move (literalValue 0) (reg RAX)
                          ++ setBitIf Equ


-- | Output asm for binary operators
binary :: String -> String -> BinaryOp -> Int -> Int -> GenState String
binary load1 load2 binOp lab1 lab2 =
        case binOp of
             Plus        -> pure $ computeAdd load1 load2
             Minus       -> pure $ computeSub load1 load2
             Multiply    -> pure $ computeMul load1 load2
             Divide      -> pure $ computeDiv load1 load2
             Modulo      -> pure $ computeMod load1 load2
             Equal       -> pure $ comparison load1 load2 ++ setBitIf Equ
             NotEqual    -> pure $ comparison load1 load2 ++ setBitIf NEqu
             GreaterThan -> pure $ comparison load1 load2 ++ setBitIf GThan
             LessThan    -> pure $ comparison load1 load2 ++ setBitIf LThan
             GThanOrEqu  -> pure $ comparison load1 load2 ++ setBitIf GThanE
             LThanOrEqu  -> pure $ comparison load1 load2 ++ setBitIf LThanE
             LogicalOR   -> pure $ logicalOR load1 load2 lab1 lab2
             LogicalAND  -> pure $ logicalAND load1 load2 lab1 lab2
             _           -> throwError $ GeneratorError (BinOpError binOp)


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


-- | Pass argument to function
passArgument:: String -> Int -> GenState String
passArgument toLoad pos = pure $ toLoad ++ putInRegister pos


putInRegister :: Int -> String
putInRegister r = move (reg RAX) (selectRegister r)


getFromRegister :: Int -> String
getFromRegister r = move (selectRegister r) (reg RAX)


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


-- | Output asm for an initialized global variable
initializedGlobal :: String -> String -> String
initializedGlobal label val =
        declareGlobl label
        ++ section DATA
        ++ align
        ++ globlLabel label
        ++ asLong val
        ++ section TEXT


-- | Output asm for an uninitialized global variable
uninitializedGlobal :: String -> String
uninitializedGlobal label =
        declareGlobl label
        ++ section BSS
        ++ align
        ++ globlLabel label
        ++ section TEXT


-- | Output asm for all uninitialized global variables
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


-- | Store the value of a global variable
storeGlobal :: String -> String
storeGlobal label =
        move (reg RAX) (fromInstructionPointer label)


-- Pointers

-- | Load a dereferenced pointer value
derefLoad :: Maybe Int -> Maybe Int -> Maybe String -> String
derefLoad (Just off) _ _ = derefLoadLocal off
derefLoad _ (Just pos) _ = derefLoadParam pos
derefLoad _ _ (Just lab) = derefLoadGlobal lab
derefLoad _ _ _          = undefined


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


derefLoadParam :: Int -> String
derefLoadParam r =
        move (valueFromAddressIn . selectRegister $ r) (reg RAX)


derefLoadGlobal :: String -> String
derefLoadGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


-- | Store a dereferenced pointer value
derefStore :: Maybe Int -> Maybe Int -> Maybe String -> String
derefStore (Just off) _ _ = derefStoreLocal off
derefStore _ (Just pos) _ = derefStoreParam pos
derefStore _ _ (Just lab) = derefStoreGlobal lab
derefStore _ _ _          = undefined


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (reg RAX) (addressIn scratch)


derefStoreParam :: Int -> String
derefStoreParam r =
        move (reg RAX) (addressIn . selectRegister $ r)


derefStoreGlobal :: String -> String
derefStoreGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (reg RAX) (addressIn scratch)


-- | Load the address of a variable
addressOf :: Maybe Int -> Maybe String -> String
addressOf (Just off) _ = varAddressLoad off
addressOf _ (Just lab) = varAddressLoadGlobal lab
addressOf _ _          = undefined


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) (reg RAX)


-- | Store the address of a local variable
varAddressStore :: Int -> String
varAddressStore offset = move (reg RAX) (fromBasePointer offset)


-- | Store the address of a global variable
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


-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"

runInit :: String -> String
runInit "main" = "jmp init\n" ++ "init_done:\n"
runInit _      = ""

-- | Empty output
noOutput :: GenState String
noOutput = pure ""


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

data Set = Equ
         | NEqu
         | GThan
         | GThanE
         | LThan
         | LThanE
         deriving (Eq)

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

data Jump = JMP
          | JE
          | JNE
          deriving (Eq)

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

data Section = TEXT
             | DATA
             | BSS
             deriving (Eq)

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

data Register = RAX
              | RBP
              | RIP
              | RSP
              | RDI
              | RSI
              | RDX
              | RCX
              | R8
              | R9
              | R12
              deriving (Eq)

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
