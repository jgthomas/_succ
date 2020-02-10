
module AsmVariables
        (storeGlobal,
         decNoAssign,
         assign,
         loadVariable,
         saveGlobal,
         varOnStack,
         derefLoad,
         derefStore,
         addressOf,
         varAddressStore,
         varAddressStoreGlobal
        ) where


import AsmShared   (literalValue, loadValue)
import Error       (CompilerError (ImpossibleError))
import GenState    (GenState, throwError)
import Instruction (loadAddOf, move, sub)
import Register    (Register (..), reg, scratch, selectRegister)


-- | Store the value of a global variable
storeGlobal :: String -> String -> GenState String
storeGlobal toAssign label = pure $ toAssign ++ saveGlobal label


-- | Output asm for a declaration with no assignment
decNoAssign :: Int -> Int -> GenState String
decNoAssign off adj = pure $
        loadValue 0
        ++ declare off adj


declare :: Int -> Int -> String
declare off adj =
        varOnStack off
        ++ adjustStackPointer adj


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literalValue offset) (reg RSP)


-- | Output asm for an assignment
assign :: String -> Int -> Int -> String
assign toAssign off adj =
        toAssign
        ++ declare off adj


-- | Load a variable value
loadVariable :: Maybe Int -> Maybe Int -> Maybe String -> GenState String
loadVariable (Just off) _ _ = pure $ varOffStack off
loadVariable _ (Just pos) _ = pure $ getFromRegister pos
loadVariable _ _ (Just lab) = pure $ loadGlobal lab
loadVariable _ _ _          = throwError ImpossibleError


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
loadGlobal :: String -> String
loadGlobal label =
        move (fromInstructionPointer label) (reg RAX)


-- | Load a dereferenced pointer value
derefLoad :: Maybe Int -> Maybe Int -> Maybe String -> GenState String
derefLoad (Just off) _ _ = pure $ derefLoadLocal off
derefLoad _ (Just pos) _ = pure $ derefLoadParam pos
derefLoad _ _ (Just lab) = pure $ derefLoadGlobal lab
derefLoad _ _ _          = throwError ImpossibleError


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
           -> GenState String
derefStore val (Just off) _ _ = pure $ val ++ derefStoreLocal off
derefStore val _ (Just pos) _ = pure $ val ++ derefStoreParam pos
derefStore val _ _ (Just lab) = pure $ val ++ derefStoreGlobal lab
derefStore _ _ _ _            = throwError ImpossibleError


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
addressOf :: Maybe Int -> Maybe String -> GenState String
addressOf (Just off) _ = pure $ varAddressLoad off
addressOf _ (Just lab) = pure $ varAddressLoadGlobal lab
addressOf _ _          = throwError ImpossibleError


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> String
varAddressLoadGlobal label = loadAddOf (fromInstructionPointer label) (reg RAX)


-- | Store the address of a local variable
varAddressStore :: String -> Int -> GenState String
varAddressStore value offset = pure $
        value
        ++ move (reg RAX) (fromBasePointer offset)


-- | Store the address of a global variable
varAddressStoreGlobal :: String -> String -> GenState String
varAddressStoreGlobal value label = pure $
        value
        ++ move (reg RAX) (fromInstructionPointer label)


addressIn :: String -> String
addressIn s = indirectAddressing s


valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"


-- | Save a local variable
varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


-- | Save a global variable
saveGlobal :: String -> String
saveGlobal label = move (reg RAX) (fromInstructionPointer label)


fromInstructionPointer :: String -> String
fromInstructionPointer lab = relAddress lab (reg RIP)


relAddress :: String -> String -> String
relAddress offset base = offset ++ indirectAddressing base


fromBasePointer :: Int -> String
fromBasePointer n = relAddress (show n) (reg RBP)
