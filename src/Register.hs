
module Register where


import Instruction (pop, push)


data Register = RAX
              | RBP
              | RIP
              | RSP
              | RDI
              | RSI
              | RDX
              | RCX
              | R8
              | R9
              | R12
              deriving (Eq)


reg :: Register -> String
reg r = case r of
             RAX -> "%rax"
             RBP -> "%rbp"
             RIP -> "%rip"
             RSP -> "%rsp"
             RDI -> "%rdi"
             RSI -> "%rsi"
             RDX -> "%rdx"
             RCX -> "%rcx"
             R8  -> "%r8"
             R9  -> "%r9"
             R12 -> "%r12"


scratch :: String
scratch = reg R12


regModResult :: String
regModResult = reg RDX


allScratch :: [Register]
allScratch = [R12]


params :: [Register]
params = [RDI,
          RSI,
          RDX,
          RCX,
          R8,
          R9
         ]


saveRegisters :: [Register] -> String
saveRegisters rs = concatMap (push . reg) rs


restoreRegisters :: [Register] -> String
restoreRegisters rs = concatMap pop . reverse . map reg $ rs


saveCallerRegisters :: String
saveCallerRegisters = saveRegisters params


restoreCallerRegisters :: String
restoreCallerRegisters = restoreRegisters params


selectRegister :: Int -> String
selectRegister n
        | n == 0 = reg RDI
        | n == 1 = reg RSI
        | n == 2 = reg RDX
        | n == 3 = reg RCX
        | n == 4 = reg R8
        | n == 5 = reg R9
        | otherwise = undefined
