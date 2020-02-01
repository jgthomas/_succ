
module Type where


data Type = IntVar
          | IntPointer
          | Label
          deriving (Eq)


instance Show Type where
        show IntVar     = "int"
        show IntPointer = "int *"
        show Label      = "@label"
