{-|
Module       : ASM
Description  : Builds strings of assembly code

Generates the strings of x86-64 assembly code.
-}
module ASM
        (module AsmBinary,
         module AsmUnary,
         module AsmTernary,
         module AsmFunction,
         module AsmPointers,
         module AsmStatement,
         loadLiteral,
         functionCall,
         decNoAssign,
         assign,
         setGotoPoint,
         passArgument,
         initializedGlobal,
         uninitializedGlobal,
         storeGlobal,
         loadVariable,
         outputInit,
         noOutput
        ) where


import AsmBinary    (binary)
import AsmFunction  (function, mainNoReturn, returnValue)
import AsmPointers  (addressOf, derefLoad, derefStore, varAddressStore,
                     varAddressStoreGlobal)
import AsmStatement (doWhile, forLoop, ifElse, ifOnly, while)
import AsmTernary   (ternary)
import AsmUnary     (unary)
import AsmVariables
import Assembly     (Jump (..), Section (..))
import Directive
import Error        (CompilerError (ImpossibleError))
import GenState     (GenState, throwError)
import Instruction
import Register


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


varOffStack :: Int -> String
varOffStack offset = move (fromBasePointer offset) (reg RAX)


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literalValue offset) (reg RSP)


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


-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"


-- | Empty output
noOutput :: GenState String
noOutput = pure empty
