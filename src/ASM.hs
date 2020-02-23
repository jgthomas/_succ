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
         module AsmStatement,
         module AsmVariables,
         module Directive,
         loadLiteral,
         setGotoPoint,
         noOutput
        ) where


import AsmBinary
import AsmFunction
import AsmStatement
import AsmTernary
import AsmUnary
import AsmVariables
import Directive    hiding (declareGlobl, emitLabel, globlLabel)
import Instruction  (Jump (..), emitJump, literal, move)
import Register     (Register (..), reg)


-- | Output asm for jump
setGotoPoint :: Int -> String
setGotoPoint target = emitJump JMP target


-- | Load a literal value into return register
loadLiteral :: Int -> String
loadLiteral n = move (literal n) (reg RAX)


-- | Empty output
noOutput :: String
noOutput = ""
