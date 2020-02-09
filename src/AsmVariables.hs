
module AsmVariables where


import Instruction (comp, move)
import Register


varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)


fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)


addressIn :: String -> String
addressIn s = indirectAddressing s


valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s


relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


literalValue :: Int -> String
literalValue n = "$" ++ show n


saveGlobal :: String -> String
saveGlobal label = move (reg RAX) (fromInstructionPointer label)


empty :: String
empty = ""


testResult :: String
testResult = comp (literalValue 0) (reg RAX)


loadValue :: Int -> String
loadValue n = move (literalValue n) (reg RAX)
