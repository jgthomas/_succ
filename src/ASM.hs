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
         module AsmBinary,
         module AsmUnary,
         module AsmTernary,
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
         noOutput
        ) where


import AsmBinary    (binary)
import AsmTernary   (ternary)
import AsmUnary     (unary)
import AsmVariables
import Assembly     (Jump (..), Section (..))
import Directive
import Error        (CompilerError (ImpossibleError))
import GenState     (GenState, throwError)
import Instruction
import Register


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
returnValue :: String -> String
returnValue rtn = rtn ++ returnStatement


saveBasePointer :: String
saveBasePointer = push (reg RBP)
                  ++ move (reg RSP) (reg RBP)


restoreBasePointer :: String
restoreBasePointer = move (reg RBP) (reg RSP)
                     ++ pop (reg RBP)


-- | Output asm for a function call
functionCall :: String -> String -> String
functionCall name args =
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
assign :: String -> Int -> Int -> String
assign toAssign off adj =
        toAssign
        ++ declare off adj


declare :: Int -> Int -> String
declare off adj =
        varOnStack off
        ++ adjustStackPointer adj


-- | Output asm for jump
setGotoPoint :: Int -> String
setGotoPoint target = emitJump JMP target


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
initializedGlobal :: String -> String -> GenState String
initializedGlobal label val = pure $
        declareGlobl label
        ++ section DATA
        ++ align
        ++ globlLabel label
        ++ asLong val
        ++ section TEXT


-- | Output asm for an uninitialized global variable
uninitializedGlobal :: String -> GenState String
uninitializedGlobal label = pure $
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
        move (fromInstructionPointer label) (reg RAX)


-- | Store the value of a global variable
storeGlobal :: String -> String -> GenState String
storeGlobal toAssign label = pure $ toAssign ++ saveGlobal label


-- Pointers

-- | Load a dereferenced pointer value
derefLoad :: Maybe Int -> Maybe Int -> Maybe String -> GenState String
derefLoad (Just off) _ _ = pure $ derefLoadLocal off
derefLoad _ (Just pos) _ = pure $ derefLoadParam pos
derefLoad _ _ (Just lab) = pure $ derefLoadGlobal lab
derefLoad _ _ _          = throwError ImpossibleError


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
derefStore :: String
           -> Maybe Int
           -> Maybe Int
           -> Maybe String
           -> GenState String
derefStore val (Just off) _ _ = pure $ val ++ derefStoreLocal off
derefStore val _ (Just pos) _ = pure $ val ++ derefStoreParam pos
derefStore val _ _ (Just lab) = pure $ val ++ derefStoreGlobal lab
derefStore _ _ _ _            = throwError ImpossibleError


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
addressOf :: Maybe Int -> Maybe String -> GenState String
addressOf (Just off) _ = pure $ varAddressLoad off
addressOf _ (Just lab) = pure $ varAddressLoadGlobal lab
addressOf _ _          = throwError ImpossibleError


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) (reg RAX)


-- | Store the address of a local variable
varAddressStore :: String -> Int -> GenState String
varAddressStore value offset = pure $
        value
        ++ move (reg RAX) (fromBasePointer offset)


-- | Store the address of a global variable
varAddressStoreGlobal :: String -> String -> GenState String
varAddressStoreGlobal value label = pure $
        value
        ++ move (reg RAX) (fromInstructionPointer label)


-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"

runInit :: String -> String
runInit "main" = "jmp init\n" ++ "init_done:\n"
runInit _      = ""


-- | Empty output
noOutput :: GenState String
noOutput = pure empty
