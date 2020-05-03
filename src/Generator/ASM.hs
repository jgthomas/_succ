{-|
Module       : ASM
Description  : Builds strings of assembly code

Generates the strings of x86-64 assembly code.
-}
module Generator.ASM
        (module Generator.AsmBinary,
         module Generator.AsmUnary,
         module Generator.AsmTernary,
         module Generator.AsmFunction,
         module Generator.AsmStatement,
         module Generator.AsmVariables,
         module Generator.Directive,
         module Generator.Instruction,
         noOutput
        ) where


import Generator.AsmBinary
import Generator.AsmFunction
import Generator.AsmStatement
import Generator.AsmTernary
import Generator.AsmUnary
import Generator.AsmVariables
import Generator.Directive    (initializedGlobal, outputInit,
                               uninitializedGlobal)
import Generator.Instruction  (setGotoPoint)


-- | Empty output
noOutput :: String
noOutput = ""
