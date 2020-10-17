{-# LANGUAGE DeriveDataTypeable #-}

module Types.Type where

import Data.Data (Data)

data Type
  = IntVar
  | IntPointer
  | IntArray
  | Label
  deriving (Eq, Data)

instance Show Type where
  show IntVar = "int"
  show IntPointer = "int *"
  show IntArray = "int []"
  show Label = "@label"
