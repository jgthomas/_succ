
module Builder.BuildVariables where


import Builder.Instruction (literal, move)
import Builder.Register    (Register (..), reg, selectRegister)
import Types.Variables     (VarType (..))


-- | Load a literal value into return register
loadLiteral :: Int -> String
loadLiteral n = move (literal n) (reg RAX)


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


fromInstructionPointerOffset :: String -> Int -> String
fromInstructionPointerOffset lab off =
        lab ++ "+" ++ show off ++ indirectAddressing (reg RIP)


fromBasePointer :: Int -> String
fromBasePointer n = show n ++ indirectAddressing (reg RBP)


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"
