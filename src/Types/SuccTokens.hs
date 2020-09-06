{-|
Module       : SuccTokens
Description  : General compiler data types

Data types used to direct the internal flow of the compilation process.
-}
module Types.SuccTokens where


-- | Compiler stages
data Stage = Input
           | Lexer
           | Parser
           | Schema
           | State
           | Output
           deriving (Eq)


-- | Debugging options
data Debug = DebugOn
           | DebugLexer
           | DebugParser
           | DebugState
           | DebugSchema
           | DebugAsm
           | DebugCode
           | DebugTrees
           | DebugOff


-- | Optimisation settings
data Optimise = OptimiseOn
              | OptimiseOff


-- | Categories of top level items
data TopLevelItem = Function
                  | InitialisedVariable
                  | UninitialisedVariable
                  | InitialisedPointer
