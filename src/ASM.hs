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
         module AsmFuncCall,
         module AsmVariables,
         loadLiteral,
         setGotoPoint,
         initializedGlobal,
         uninitializedGlobal,
         outputInit,
         noOutput
        ) where


import AsmBinary    (binary)
import AsmFuncCall  (functionCall, passArgument)
import AsmFunction  (function, mainNoReturn, returnValue)
import AsmPointers  (addressOf, derefLoad, derefStore, varAddressStore,
                     varAddressStoreGlobal)
import AsmShared    (empty, loadValue)
import AsmStatement (doWhile, forLoop, ifElse, ifOnly, while)
import AsmTernary   (ternary)
import AsmUnary     (unary)
import AsmVariables (assign, decNoAssign, loadVariable, storeGlobal)
import Assembly     (Jump (..), Section (..))
import Directive
import GenState     (GenState)
import Instruction


-- | Output asm for jump
setGotoPoint :: Int -> String
setGotoPoint target = emitJump JMP target


-- | Load a literal value into return register
loadLiteral :: Int -> GenState String
loadLiteral n = pure . loadValue $ n


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


-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"


-- | Empty output
noOutput :: GenState String
noOutput = pure empty
