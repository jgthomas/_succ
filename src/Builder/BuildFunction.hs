
module Builder.BuildFunction where


import Builder.Directive   (declareGlobl, globlLabel)
import Builder.Instruction (move, pop, push, returnControl)
import Builder.Register    (Register (..), allScratch, reg)


funcPrologue :: String -> String
funcPrologue funcName =
        declareGlobl funcName
        ++ globlLabel funcName
        ++ saveBasePointer
        ++ saveRegisters allScratch


funcEpilogue :: String
funcEpilogue =
        restoreRegisters allScratch
        ++ restoreBasePointer
        ++ returnControl


saveRegisters :: [Register] -> String
saveRegisters rs = concatMap (push . reg) rs


restoreRegisters :: [Register] -> String
restoreRegisters rs = concatMap pop . reverse . map reg $ rs


saveBasePointer :: String
saveBasePointer = push (reg RBP)
                  ++ move (reg RSP) (reg RBP)


restoreBasePointer :: String
restoreBasePointer = move (reg RBP) (reg RSP)
                     ++ pop (reg RBP)
