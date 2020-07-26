
module Builder.BuildVariables
        (loadLiteral,
         storeVariable,
         loadVariable,
         addressOf,
         derefLoad,
         declareGlobal,
         postDeclareAction

        ) where


import Builder.Directive   (initializedGlobal, uninitializedGlobal)
import Builder.Instruction (literal, loadAddOf, move, sub)
import Builder.Register    (Register (..), reg, scratch, selectRegister)
import Types.Variables     (VarType (..))


-- | Load a literal value into return register
loadLiteral :: Int -> String
loadLiteral n = move (literal n) (reg RAX)


declareGlobal :: VarType -> Int -> String
declareGlobal (GlobalVar label _) 0 = uninitializedGlobal label
declareGlobal (GlobalVar label _) n = initializedGlobal label (show n)
declareGlobal _ _                   = undefined


postDeclareAction :: VarType -> String
postDeclareAction GlobalVar{}       = ""
postDeclareAction ParamVar{}        = ""
postDeclareAction (LocalVar _ _ sp) = adjustStackPointer sp


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        move (reg RBP) (reg RSP)
        ++ sub (literal offset) (reg RSP)


-- | Store a variable value currently held in %rax
storeVariable :: VarType -> String
storeVariable (LocalVar n m _) = varOnStack (n + m)
storeVariable (ParamVar n _)   = updateParam n
storeVariable (GlobalVar s o)  = saveGlobal s o


varOnStack :: Int -> String
varOnStack offset = move (reg RAX) (fromBasePointer offset)


updateParam :: Int -> String
updateParam n = move (reg RAX) (selectRegister n)


saveGlobal :: String -> Int -> String
saveGlobal label offset =
        move (reg RAX) (fromInstructionPointerOffset label offset)


-- | Load a variable value to %rax
loadVariable :: VarType -> String
loadVariable (LocalVar n m _) = varOffStack (n + m)
loadVariable (ParamVar n _)   = getFromRegister n
loadVariable (GlobalVar s o)  = loadGlobal s o


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


-- | Load the address of a variable
addressOf :: VarType -> String
addressOf (LocalVar n m _) = varAddressLoad (n + m)
addressOf (ParamVar _ _)   = undefined
addressOf (GlobalVar s o)  = varAddressLoadGlobal s o


varAddressLoad :: Int -> String
varAddressLoad offset = loadAddOf (fromBasePointer offset) (reg RAX)


varAddressLoadGlobal :: String -> Int -> String
varAddressLoadGlobal label offset =
        loadAddOf (fromInstructionPointerOffset label offset) (reg RAX)


-- | Load a dereferenced pointer value
derefLoad :: VarType -> String
derefLoad (LocalVar n m _) = derefLoadLocal (n + m)
derefLoad (ParamVar n _)   = derefLoadParam n
derefLoad (GlobalVar s o)  = derefLoadGlobal s o


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


fromInstructionPointerOffset :: String -> Int -> String
fromInstructionPointerOffset lab off =
        lab ++ "+" ++ show off ++ indirectAddressing (reg RIP)


valueFromAddressIn :: String -> String
valueFromAddressIn s = indirectAddressing s


fromBasePointer :: Int -> String
fromBasePointer n = show n ++ indirectAddressing (reg RBP)


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"
