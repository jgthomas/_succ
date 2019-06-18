
module ASM_Tokens (Jump(..), Section(..)) where


data Jump = JMP
          | JE
          | JNE
          deriving Eq


data Section = TEXT
             | DATA
             | BSS
             deriving Eq
