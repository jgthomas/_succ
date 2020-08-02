{-# LANGUAGE DeriveDataTypeable #-}

module Types.Variables where


import Data.Data (Data)


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
