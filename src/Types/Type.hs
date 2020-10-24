{-# LANGUAGE DeriveDataTypeable #-}

module Types.Type where

import Data.Data (Data)

data Type
  = IntVar
  | IntPointer
  | IntArray Int
  | Label
  deriving (Eq, Data)

instance Show Type where
  show IntVar = "int"
  show IntPointer = "int *"
  show (IntArray n) = "int [] " ++ show n
  show Label = "@label"
