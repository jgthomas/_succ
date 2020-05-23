{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug, setDebugLevel) where


import Debug.DebugPrint
import Types.SuccTokens (Debug (..), Stage (..))


-- | Print debugging output
debug :: Show a => Debug -> Stage -> IO a -> IO a
debug DebugOff _ x    = x
debug DebugOn stage x = debugIt stage x


-- | Set debug level based on input from user
setDebugLevel :: Maybe String -> Debug
setDebugLevel Nothing = DebugOff
setDebugLevel (Just dbug)
        | dbug == "debug" = DebugOn
        | otherwise       = DebugOff


debugIt :: Show a => Stage -> IO a -> IO a
debugIt stage x = do
        y <- x
        case stage of
             Input  -> printSourceFile inputTitle y
             Lexer  -> printDataTypeSimple lexTitle y
             Parser -> printDataType parTitle y
             State  -> printDataType stateTitle y
             Output -> printSourceFile outTitle y
        x
        where
                inputTitle = "C CODE"
                lexTitle   = "LEXED TOKENS"
                parTitle   = "ABSTRACT SYNTAX TREE"
                stateTitle = "SYMBOL TABLE"
                outTitle   = "ASSEMBLY CODE"
