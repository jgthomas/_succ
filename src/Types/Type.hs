
module Types.Type where


data Type = IntVar
          | IntPointer
          | IntArray
          | Label
          deriving (Eq)


instance Show Type where
        show IntVar     = "int"
        show IntPointer = "int *"
        show IntArray   = "int []"
        show Label      = "@label"
