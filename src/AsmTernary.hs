
module AsmTernary (ternary) where


import Directive   (emitLabel)
import GenState    (GenState)
import Instruction (Jump (..), comp, emitJump, literal)
import Register    (Register (..), reg)


-- | Output asm for the ternary operator
ternary :: String -> String -> String -> Int -> Int -> GenState String
ternary test true false trueLab falseLab = pure $
        test
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE falseLab
        ++ true
        ++ emitJump JMP trueLab
        ++ emitLabel falseLab
        ++ false
        ++ emitLabel trueLab


