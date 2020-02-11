
module AsmShared where


import Instruction (comp, move)
import Register    (Register (..), reg)


literalValue :: Int -> String
literalValue n = "$" ++ show n


loadValue :: Int -> String
loadValue n = move (literalValue n) (reg RAX)


testResult :: String
testResult = comp (literalValue 0) (reg RAX)
