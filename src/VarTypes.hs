
module VarTypes
        (Type(..)
        ) where


data Type = IntVar
          | IntPointer
          | Label
          deriving (Show, Eq)
