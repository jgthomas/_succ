module Builder.Directive
  ( outputInit,
    initializedGlobal,
    uninitializedGlobal,
    uninitializedGlobalArray,
    declareGlobl,
    globlLabel,
    emitLabel,
  )
where

data Section
  = TEXT
  | DATA
  | BSS
  deriving (Eq)

-- | Setup initialisation block
outputInit :: String -> String
outputInit toInit = "init:\n" ++ toInit ++ "jmp init_done\n"

-- | Output asm for an initialized global variable
initializedGlobal :: String -> String -> String
initializedGlobal label val =
  declareGlobl label
    ++ section DATA
    ++ align
    ++ globlLabel label
    ++ asLong val
    ++ section TEXT

-- | Output asm for an uninitialized global variable
uninitializedGlobal :: String -> String
uninitializedGlobal label =
  declareGlobl label
    ++ section BSS
    ++ align
    ++ globlLabel label
    ++ section TEXT

-- | Output asm for an uninitialized global array
uninitializedGlobalArray :: String -> Int -> String
uninitializedGlobalArray label n =
  declareGlobl label
    ++ section BSS
    ++ align
    ++ globlLabel label
    ++ space n
    ++ section TEXT

-- | Create a label
emitLabel :: Int -> String
emitLabel n = "_label_" ++ show n ++ ":\n"

-- | Declare .globl
declareGlobl :: String -> String
declareGlobl name = ".globl " ++ name ++ "\n"

-- | Create a .globl label
globlLabel :: String -> String
globlLabel name = name ++ ":\n"

space :: Int -> String
space n = ".space " ++ show (n * 4) ++ "\n"

section :: Section -> String
section sect =
  case sect of
    TEXT -> ".text\n"
    DATA -> ".data\n"
    BSS -> ".bss\n"

align :: String
align = ".align 4\n"

asLong :: String -> String
asLong l = ".long " ++ l ++ "\n"
