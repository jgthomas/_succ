
module Register where


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
