{-|
Module       : ASM
Description  : Builds strings of assembly code

Generates the strings of x86-64 assembly code.
-}
module Asm.ASM
        (module Asm.AsmBinary,
         module Asm.AsmUnary,
         module Asm.AsmTernary,
         module Asm.AsmFunction,
         module Asm.AsmStatement,
         module Asm.AsmVariables,
         module Asm.Directive,
         module Asm.Instruction,
         noOutput
        ) where


import Asm.AsmBinary
import Asm.AsmFunction
import Asm.AsmStatement
import Asm.AsmTernary
import Asm.AsmUnary
import Asm.AsmVariables
import Asm.Directive    (initializedGlobal, outputInit, uninitializedGlobal)
import Asm.Instruction  (setGotoPoint)


-- | Empty output
noOutput :: String
noOutput = ""
