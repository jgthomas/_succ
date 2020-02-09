
module AsmShared where


import Instruction (move)
import Register    (Register (..), reg)


empty :: String
empty = ""


literalValue :: Int -> String
literalValue n = "$" ++ show n


loadValue :: Int -> String
loadValue n = move (literalValue n) (reg RAX)
