{-|
Module       : SuccTokens
Description  : General compiler data types

Data types used to direct the internal flow of the compilation process.
-}
module SuccTokens where


-- | Debug status
data Debug = DebugOn
           | DebugOff
           deriving (Eq)


-- | Compiler stage
data Stage = Input
           | Lexer
           | Parser
           | State
           | Output
           deriving (Eq)
