
module ASM_Tokens
        (Jump(..),
         Section(..),
         Set(..),
         Register(..)
        ) where


data Jump = JMP
          | JE
          | JNE
          deriving Eq


data Section = TEXT
             | DATA
             | BSS
             deriving Eq


data Set = Equ
         | NEqu
         | GThan
         | GThanE
         | LThan
         | LThanE
         deriving Eq


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
