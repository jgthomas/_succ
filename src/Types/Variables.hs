{-# LANGUAGE DeriveDataTypeable #-}

module Types.Variables where


import           Data.Data (Data)
import qualified Data.Map  as M


data Scope = Global
           | Local
           deriving (Eq, Show, Data)


data VarLookup = NotFound
               | VarType VarType
               deriving (Eq, Show)


data VarType = LocalVar Int Int Int
             | ParamVar Int Int
             | GlobalVar String Int
             deriving (Eq, Show, Data)


data VarValue = SingleValue Int
              | MultiValue (M.Map Int Int)
              | AddressValue Int
              | UntrackedValue
              deriving (Eq, Show, Data)
