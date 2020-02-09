
module Directive where


import Assembly (Section (..))


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
