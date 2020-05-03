
module Generator.AsmTernary (ternary) where


import Generator.Directive   (emitLabel)
import Generator.Instruction (Jump (..), comp, emitJump, literal)
import Generator.Register    (Register (..), reg)


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


