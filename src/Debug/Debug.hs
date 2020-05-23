{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug, setDebugLevel, debugPair) where


import Debug.DebugPrint
import Types.SuccTokens (Debug (..), Stage (..))


-- | Print debugging output
debug :: Show a => Maybe String -> Stage -> (IO a -> IO a)
debug debugSet = debugSingle (setDebugLevel debugSet)


-- | Print debugging output for pair of values
debugPair :: (Show a, Show b) => Maybe String
              -> (Stage, Stage)
              -> (IO (a, b) -> IO (a, b))
debugPair debugSet (s1, s2) = debugMultiple (setDebugLevel debugSet) (s1, s2)


-- | Set debug level based on input from user
setDebugLevel :: Maybe String -> Debug
setDebugLevel Nothing = DebugOff
setDebugLevel (Just dbug)
        | dbug == "debug" = DebugOn
        | otherwise       = DebugOff


debugSingle :: Show a => Debug -> Stage -> IO a -> IO a
debugSingle DebugOff _ x    = x
debugSingle DebugOn stage x = debugIt stage x


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
