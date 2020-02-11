
module AsmTernary (ternary) where


import Directive   (emitLabel)
import Instruction (Jump (..), comp, emitJump, literal)
import Register    (Register (..), reg)


-- | Output asm for the ternary operator
ternary :: String -> String -> String -> Int -> Int -> String
ternary test true false trueLab falseLab =
        test
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE falseLab
        ++ true
        ++ emitJump JMP trueLab
        ++ emitLabel falseLab
        ++ false
        ++ emitLabel trueLab


