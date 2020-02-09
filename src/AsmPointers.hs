
module AsmPointers where


import AsmVariables (addressIn, fromBasePointer, fromInstructionPointer,
                     valueFromAddressIn)
import Error        (CompilerError (ImpossibleError))
import GenState     (GenState, throwError)
import Instruction
import Register


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
