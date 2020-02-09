
module Directive where


import Assembly (Section (..))
import GenState (GenState)


-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"


-- | Output asm for an initialized global variable
initializedGlobal :: String -> String -> GenState String
initializedGlobal label val = pure $
        declareGlobl label
        ++ section DATA
        ++ align
        ++ globlLabel label
        ++ asLong val
        ++ section TEXT


-- | Output asm for an uninitialized global variable
uninitializedGlobal :: String -> GenState String
uninitializedGlobal label = pure $
        declareGlobl label
        ++ section BSS
        ++ align
        ++ globlLabel label
        ++ section TEXT


declareGlobl :: String -> String
declareGlobl name = ".globl " ++ name ++ "\n"


globlLabel :: String -> String
globlLabel name = name ++ ":\n"


section :: Section -> String
section sect =
        case sect of
             TEXT -> ".text\n"
             DATA -> ".data\n"
             BSS  -> ".bss\n"


align :: String
align = ".align 4\n"


asLong :: String -> String
asLong l = ".long " ++ l ++ "\n"


emitLabel :: Int -> String
emitLabel n = "_label_" ++ show n ++ ":\n"
