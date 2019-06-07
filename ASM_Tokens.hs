
module ASM_Tokens (Jump(..)) where


data Jump = JMP
          | JE
          | JNE
          deriving Eq
