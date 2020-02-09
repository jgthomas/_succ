
module Assembly where


data Set = Equ
         | NEqu
         | GThan
         | GThanE
         | LThan
         | LThanE
         deriving (Eq)


data Jump = JMP
          | JE
          | JNE
          deriving (Eq)
