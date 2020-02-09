
module AsmFuncCall where


import GenState    (GenState)
import Instruction (call, move)
import Register


-- | Output asm for a function call
functionCall :: String -> String -> String
functionCall name args =
        saveCallerRegisters
        ++ args
        ++ makeFunctionCall name
        ++ restoreCallerRegisters


makeFunctionCall :: String -> String
makeFunctionCall funcName = call funcName


-- | Pass argument to function
passArgument:: String -> Int -> GenState String
passArgument toLoad pos = pure $ toLoad ++ putInRegister pos


putInRegister :: Int -> String
putInRegister r = move (reg RAX) (selectRegister r)


restoreCallerRegisters :: String
restoreCallerRegisters = restoreRegisters params
