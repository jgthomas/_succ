
module AsmShared where


import Instruction (comp, move)
import Register    (Register (..), reg)


empty :: String
empty = ""


literalValue :: Int -> String
literalValue n = "$" ++ show n


loadValue :: Int -> String
loadValue n = move (literalValue n) (reg RAX)


testResult :: String
testResult = comp (literalValue 0) (reg RAX)


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


saveGlobal :: String -> String
saveGlobal label = move (reg RAX) (fromInstructionPointer label)


fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)


relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base


fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)
