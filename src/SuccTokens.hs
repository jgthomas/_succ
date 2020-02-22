
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
