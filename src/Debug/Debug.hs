{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug) where


import Debug.DebugPrint
import Types.SuccTokens (Debug (..), Stage (..))


-- | Print debugging output
debug :: Show a => Debug -> Stage -> IO a -> IO a
debug DebugOff _ x    = x
debug DebugOn stage x = debugIt stage x


debugIt :: Show a => Stage -> IO a -> IO a
debugIt stage x = do
        y <- x
        case stage of
             Input  -> printString inputTitle y
             Lexer  -> printDataTypeSimple lexTitle y
             Parser -> printDataType parTitle y
             State  -> printDataType stateTitle y
             Output -> printString outTitle y
        x
        where
                inputTitle = "C CODE"
                lexTitle   = "LEXED TOKENS"
                parTitle   = "ABSTRACT SYNTAX TREE"
                stateTitle = "SYMBOL TABLE"
                outTitle   = "ASSEMBLY CODE"
