
module AsmVariables
        (storeGlobal,
         decNoAssign,
         assign,
         loadVariable
        ) where


import AsmShared   (fromBasePointer, fromInstructionPointer, literalValue,
                    loadValue, saveGlobal, varOnStack)
import Error       (CompilerError (ImpossibleError))
import GenState    (GenState, throwError)
import Instruction (move, sub)
import Register


-- | Store the value of a global variable
storeGlobal :: String -> String -> GenState String
storeGlobal toAssign label = pure $ toAssign ++ saveGlobal label


-- | Output asm for a declaration with no assignment
decNoAssign :: Int -> Int -> GenState String
decNoAssign off adj = pure $
        loadValue 0
        ++ declare off adj


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


{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        move (fromInstructionPointer label) (reg RAX)


declare :: Int -> Int -> String
declare off adj =
        varOnStack off
        ++ adjustStackPointer adj


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literalValue offset) (reg RSP)
