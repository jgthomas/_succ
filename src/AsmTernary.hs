
module AsmTernary (ternary) where


import AsmShared   (testResult)
import Directive   (emitLabel)
import GenState    (GenState)
import Instruction (Jump (..), emitJump)


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


