
module AsmFunction
        (function,
         mainNoReturn,
         returnValue,
         functionCall,
         passArgument
        ) where


import Directive   (declareGlobl, globlLabel)
import Instruction (call, literal, move, pop, push, returnControl)
import Register    (Register (..), allScratch, params, reg, selectRegister)


-- | Output asm for a function
function :: String -> String -> String
function name stmts = functionInit name ++ stmts


-- | Output asm for a main function with no explicit return value
mainNoReturn :: String -> String -> String
mainNoReturn name stmts =
        functionInit name
        ++ stmts
        ++ move (literal 0) (reg RAX)
        ++ returnStatement


-- | Output asm for a return value
returnValue :: String -> String
returnValue rtn = rtn ++ returnStatement


functionInit :: String -> String
functionInit funcName =
        declareGlobl funcName
        ++ globlLabel funcName
        ++ runInit funcName
        ++ saveBasePointer
        ++ saveRegisters allScratch


returnStatement :: String
returnStatement =
        restoreRegisters allScratch
        ++ restoreBasePointer
        ++ returnControl


saveBasePointer :: String
saveBasePointer = push (reg RBP)
                  ++ move (reg RSP) (reg RBP)


restoreBasePointer :: String
restoreBasePointer = move (reg RBP) (reg RSP)
                     ++ pop (reg RBP)


runInit :: String -> String
runInit "main" = "jmp init\n" ++ "init_done:\n"
runInit _      = ""


saveRegisters :: [Register] -> String
saveRegisters rs = concatMap (push . reg) rs


restoreRegisters :: [Register] -> String
restoreRegisters rs = concatMap pop . reverse . map reg $ rs


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
passArgument:: String -> Int -> String
passArgument toLoad pos = toLoad ++ putInRegister pos


putInRegister :: Int -> String
putInRegister r = move (reg RAX) (selectRegister r)


restoreCallerRegisters :: String
restoreCallerRegisters = restoreRegisters params


saveCallerRegisters :: String
saveCallerRegisters = saveRegisters params
