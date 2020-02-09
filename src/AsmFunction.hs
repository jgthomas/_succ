
module AsmFunction (function, mainNoReturn, returnValue) where


import AsmShared   (loadValue)
import Directive   (declareGlobl, globlLabel)
import GenState    (GenState)
import Instruction (move, pop, push, returnControl)
import Register    (Register (..), allScratch, reg, restoreRegisters,
                    saveRegisters)


-- | Output asm for a function
function :: String -> String -> GenState String
function name stmts = pure $ functionInit name ++ stmts


-- | Output asm for a main function with no explicit return value
mainNoReturn :: String -> String -> GenState String
mainNoReturn name stmts = pure $
        functionInit name
        ++ stmts
        ++ loadValue 0
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
