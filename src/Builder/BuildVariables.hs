
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


fromInstructionPointerOffset :: String -> Int -> String
fromInstructionPointerOffset lab off =
        lab ++ "+" ++ show off ++ indirectAddressing (reg RIP)


fromBasePointer :: Int -> String
fromBasePointer n = show n ++ indirectAddressing (reg RBP)


indirectAddressing :: String -> String
indirectAddressing s = "(" ++ s ++ ")"
