
module Builder.BuildTernary (ternary) where


import Builder.Directive   (emitLabel)
import Builder.Instruction (Jump (..), comp, emitJump, literal)
import Builder.Register    (Register (..), reg)


-- | Output asm for the ternary operator
ternary :: String -> String -> String -> Int -> Int -> String
ternary testExpr trueExpr falseExpr trueLabNum falseLabNum =
        testExpr
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE falseLabNum
        ++ trueExpr
        ++ emitJump JMP trueLabNum
        ++ emitLabel falseLabNum
        ++ falseExpr
        ++ emitLabel trueLabNum
