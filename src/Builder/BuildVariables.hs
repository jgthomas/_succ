
module Builder.BuildVariables where


import Builder.Instruction (literal, move)
import Builder.Register    (Register (..), reg)


-- | Load a literal value into return register
loadLiteral :: Int -> String
loadLiteral n = move (literal n) (reg RAX)
