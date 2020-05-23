{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug, debugPair) where


import Debug.DebugPrint
import Types.SuccTokens (Stage (..))


data Debug = DebugOn
           | DebugLexer
           | DebugParser
           | DebugState
           | DebugAsm
           | DebugCode
           | DebugOff


-- | Print debugging output
debug :: Show a => Maybe String -> Stage -> (IO a -> IO a)
debug debugSet = debugSingle (setDebugLevel debugSet)


-- | Print debugging output for pair of values
debugPair :: (Show a, Show b) =>
          Maybe String
          -> (Stage, Stage)
          -> (IO (a, b) -> IO (a, b))
debugPair debugSet (s1, s2) = debugMultiple (setDebugLevel debugSet) (s1, s2)


setDebugLevel :: Maybe String -> Debug
setDebugLevel Nothing = DebugOff
setDebugLevel (Just dbug)
        | dbug == "debug"       = DebugOn
        | dbug == "debugLexer"  = DebugLexer
        | dbug == "debugParser" = DebugParser
        | dbug == "debugAsm"    = DebugAsm
        | dbug == "debugState"  = DebugState
        | dbug == "debugCode"   = DebugCode
        | otherwise             = DebugOff


debugSingle :: Show a => Debug -> Stage -> IO a -> IO a
debugSingle DebugCode Input x    = debugIt Input x
debugSingle DebugCode Output x   = debugIt Output x
debugSingle DebugCode _ x        = x
debugSingle DebugLexer Lexer x   = debugIt Lexer x
debugSingle DebugLexer _ x       = x
debugSingle DebugParser Parser x = debugIt Parser x
debugSingle DebugParser _ x      = x
debugSingle DebugAsm Output x    = debugIt Output x
debugSingle DebugAsm _ x         = x
debugSingle DebugState State x   = debugIt State x
debugSingle DebugState _ x       = x
debugSingle DebugOn stage x      = debugIt stage x
debugSingle DebugOff _ x         = x


debugMultiple :: (Show a, Show b) =>
              Debug
              -> (Stage, Stage)
              -> IO (a, b)
              -> IO (a, b)
debugMultiple debugLevel (s1, s2) output = do
        (p1, p2) <- output
        _ <- debugSingle debugLevel s2 (pure p2)
        _ <- debugSingle debugLevel s1 (pure p1)
        output


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
