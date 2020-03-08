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
         module Instruction,
         noOutput
        ) where


import AsmBinary
import AsmFunction
import AsmStatement
import AsmTernary
import AsmUnary
import AsmVariables
import Directive    (initializedGlobal, outputInit, uninitializedGlobal)
import Instruction  (setGotoPoint)


-- | Empty output
noOutput :: String
noOutput = ""
