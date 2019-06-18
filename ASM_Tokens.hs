
module ASM_Tokens (Jump(..), Section(..), Set(..)) where


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
