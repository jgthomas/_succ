
module AsmVariables
        (decNoAssign,
         assign,
         loadVariable,
         storeVariable,
         derefLoad,
         derefStore,
         addressOf,
         varAddressStore,
         varAddressStoreGlobal,
         loadLiteral
        ) where


import GenTokens   (VarType (..))
import Instruction (literal, loadAddOf, move, sub)
import Register    (Register (..), reg, scratch, selectRegister)


-- | Load a literal value into return register
loadLiteral :: Int -> String
loadLiteral n = move (literal n) (reg RAX)


-- | Output asm for a declaration with no assignment
decNoAssign :: Int -> Int -> String
decNoAssign off adj =
        move (literal 0) (reg RAX)
        ++ declare off adj


declare :: Int -> Int -> String
declare off adj =
        varOnStack off
        ++ adjustStackPointer adj


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literal offset) (reg RSP)


-- | Output asm for an assignment
assign :: String -> Int -> Int -> String
assign toAssign off adj =
        toAssign
        ++ declare off adj


-- | Load a variable value to %rax
loadVariable :: VarType -> String
loadVariable (LocalVar n m)  = varOffStack (n + m)
loadVariable (ParamVar n _)  = getFromRegister n
loadVariable (GlobalVar s o) = loadGlobal s o


varOffStack :: Int -> String
varOffStack offset = move (fromBasePointer offset) (reg RAX)


getFromRegister :: Int -> String
getFromRegister r = move (selectRegister r) (reg RAX)


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> Int -> String
loadGlobal label offset = move (fromInstructionPointerOffset label offset) (reg RAX)


-- | Store a variable value currently held in %rax
storeVariable :: VarType -> String
storeVariable (LocalVar n m)  = varOnStack (n + m)
storeVariable (ParamVar n _)  = updateParam n
storeVariable (GlobalVar s o) = saveGlobal s o


varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


updateParam :: Int -> String
updateParam n = move (reg RAX) (selectRegister n)


saveGlobal :: String -> Int -> String
saveGlobal label offset = move (reg RAX) (fromInstructionPointerOffset label offset)


-- | Load a dereferenced pointer value
derefLoad :: VarType -> String
derefLoad (LocalVar n m)  = derefLoadLocal (n + m)
derefLoad (ParamVar n _)  = derefLoadParam n
derefLoad (GlobalVar s o) = derefLoadGlobal s o


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


derefLoadParam :: Int -> String
derefLoadParam r =
        move (valueFromAddressIn . selectRegister $ r) (reg RAX)


derefLoadGlobal :: String -> Int -> String
derefLoadGlobal label offset =
        move (fromInstructionPointerOffset label offset) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


-- | Store a dereferenced pointer value
derefStore :: String -> VarType -> String
derefStore val (LocalVar n m)  = val ++ derefStoreLocal (n + m)
derefStore val (ParamVar n _)  = val ++ derefStoreParam n
derefStore val (GlobalVar s o) = val ++ derefStoreGlobal s o


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (reg RAX) (addressIn scratch)


derefStoreParam :: Int -> String
derefStoreParam r =
        move (reg RAX) (addressIn . selectRegister $ r)


derefStoreGlobal :: String -> Int -> String
derefStoreGlobal label offset =
        move (fromInstructionPointerOffset label offset) scratch
        ++ move (reg RAX) (addressIn scratch)


-- | Load the address of a variable
addressOf :: VarType -> String
addressOf (LocalVar n m)  = varAddressLoad (n + m)
addressOf (ParamVar _ _)  = undefined
addressOf (GlobalVar s o) = varAddressLoadGlobal s o


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> Int -> String
varAddressLoadGlobal label offset =
        loadAddOf (fromInstructionPointerOffset label offset) (reg RAX)


-- | Store the address of a local variable
varAddressStore :: String -> Int -> String
varAddressStore value offset =
        value
        ++ move (reg RAX) (fromBasePointer offset)


-- | Store the address of a global variable
varAddressStoreGlobal :: String -> String -> String
varAddressStoreGlobal value label =
        value
        ++ move (reg RAX) (fromInstructionPointer label)


addressIn :: String -> String
addressIn s = indirectAddressing s


valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


fromInstructionPointerOffset :: String -> Int -> String
fromInstructionPointerOffset lab off = lab ++ "+" ++ show off ++ indirectAddressing (reg RIP)


fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)


relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base


fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)
