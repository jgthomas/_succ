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
         module Directive,
         loadLiteral,
         setGotoPoint,
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
import Directive    (initializedGlobal, outputInit, uninitializedGlobal)
import GenState     (GenState)
import Instruction  (Jump (..), emitJump)


-- | Output asm for jump
setGotoPoint :: Int -> String
setGotoPoint target = emitJump JMP target


-- | Load a literal value into return register
loadLiteral :: Int -> GenState String
loadLiteral n = pure . loadValue $ n


-- | Empty output
noOutput :: GenState String
noOutput = pure empty
