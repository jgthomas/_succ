{-|
Module       : SuccTokens
Description  : General compiler data types

Data types used to direct the internal flow of the compilation process.
-}
module Types.SuccTokens where


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


data Optimise = OptimiseOn
              | OptimiseOff
