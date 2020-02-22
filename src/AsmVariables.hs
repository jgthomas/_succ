
module AsmVariables
        (storeGlobal,
         decNoAssign,
         assign,
         loadVariable,
         loadVar,
         storeVariable,
         saveGlobal,
         varOnStack,
         derefLoad,
         derefStore,
         addressOf,
         varAddressStore,
         varAddressStoreGlobal
        ) where


import GenTokens   (VarType (..))
import Instruction (literal, loadAddOf, move, sub)
import Register    (Register (..), reg, scratch, selectRegister)


-- | Store the value of a global variable
storeGlobal :: String -> String -> String
storeGlobal toAssign label = toAssign ++ saveGlobal label


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
loadVariable :: Maybe Int -> Maybe Int -> Maybe String -> String
loadVariable (Just off) _ _ = varOffStack off
loadVariable _ (Just pos) _ = getFromRegister pos
loadVariable _ _ (Just lab) = loadGlobal lab
loadVariable _ _ _          = ""


loadVar :: VarType -> String
loadVar (LocalVar n m)  = varOffStack (n + m)
loadVar (ParamVar n _)  = getFromRegister n
loadVar (GlobalVar s _) = loadGlobal s


varOffStack :: Int -> String
varOffStack offset = move (fromBasePointer offset) (reg RAX)


getFromRegister :: Int -> String
getFromRegister r = move (selectRegister r) (reg RAX)


-- | Store a variable value currently held in %rax
storeVariable :: Maybe Int -> Maybe String -> String
storeVariable (Just off) _ = varOnStack off
storeVariable _ (Just lab) = saveGlobal lab
storeVariable _ _          = ""


-- | Save a local variable
varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


-- | Save a global variable
saveGlobal :: String -> String
saveGlobal label = move (reg RAX) (fromInstructionPointer label)


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        move (fromInstructionPointer label) (reg RAX)


-- | Load a dereferenced pointer value
derefLoad :: Maybe Int -> Maybe Int -> Maybe String -> String
derefLoad (Just off) _ _ = derefLoadLocal off
derefLoad _ (Just pos) _ = derefLoadParam pos
derefLoad _ _ (Just lab) = derefLoadGlobal lab
derefLoad _ _ _          = ""


derefLoadLocal :: Int -> String
derefLoadLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


derefLoadParam :: Int -> String
derefLoadParam r =
        move (valueFromAddressIn . selectRegister $ r) (reg RAX)


derefLoadGlobal :: String -> String
derefLoadGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (valueFromAddressIn scratch) (reg RAX)


-- | Store a dereferenced pointer value
derefStore :: String
           -> Maybe Int
           -> Maybe Int
           -> Maybe String
           -> String
derefStore val (Just off) _ _ = val ++ derefStoreLocal off
derefStore val _ (Just pos) _ = val ++ derefStoreParam pos
derefStore val _ _ (Just lab) = val ++ derefStoreGlobal lab
derefStore _ _ _ _            = ""


derefStoreLocal :: Int -> String
derefStoreLocal offset =
        move (fromBasePointer offset) scratch
        ++ move (reg RAX) (addressIn scratch)


derefStoreParam :: Int -> String
derefStoreParam r =
        move (reg RAX) (addressIn . selectRegister $ r)


derefStoreGlobal :: String -> String
derefStoreGlobal label =
        move (fromInstructionPointer label) scratch
        ++ move (reg RAX) (addressIn scratch)


-- | Load the address of a variable
addressOf :: Maybe Int -> Maybe String -> String
addressOf (Just off) _ = varAddressLoad off
addressOf _ (Just lab) = varAddressLoadGlobal lab
addressOf _ _          = ""


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) (reg RAX)


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


fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)


relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base


fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)
