
module Builder.BuildTernary (ternary) where


import Builder.Directive    (emitLabel)
import Builder.Instruction  (Jump (..), comp, emitJump, literal)
import Builder.Register     (Register (..), reg)
import Types.AssemblySchema (Label (..))


-- | Output asm for the ternary operator
ternary :: String -> String -> String -> Label -> Label -> String
ternary testExpr trueExpr falseExpr (LocalLabel true) (LocalLabel false) =
        testExpr
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE false
        ++ trueExpr
        ++ emitJump JMP true
        ++ emitLabel false
        ++ falseExpr
        ++ emitLabel true
ternary _ _ _ _ _ = undefined
