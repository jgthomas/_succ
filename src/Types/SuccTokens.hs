{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module       : SuccTokens
Description  : General compiler data types

Data types used to direct the internal flow of the compilation process.
-}
module Types.SuccTokens where

import System.Console.CmdArgs (Data, Typeable)


-- | Compiler stage
data Stage = Input
           | Lexer
           | Parser
           | Check
           | Schema
           | State
           | Output
           deriving (Eq)


data Debug = DebugOn
           | DebugLexer
           | DebugParser
           | DebugState
           | DebugSchema
           | DebugAsm
           | DebugCode
           | DebugOff


data SuccArgs = SuccArgs {
        debug :: Bool
      , stage :: String
      , file  :: FilePath
} deriving (Show, Data, Typeable)
