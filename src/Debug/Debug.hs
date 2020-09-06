{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug, debugPair) where


import qualified Debug.DebugPrint as DebugPrint (printDebug)
import           Types.SuccTokens (Debug (..), Stage (..))


-- | Print debugging output
debug :: Show a => Debug -> Stage -> (IO a -> IO a)
debug debugOption = debugSingle debugOption


-- | Print debugging output for pair of values
debugPair :: (Show a, Show b) =>
          Debug
          -> (Stage, Stage)
          -> (IO (a, b) -> IO (a, b))
debugPair debugOption (s1, s2) = debugMultiple debugOption (s1, s2)


debugSingle :: Show a => Debug -> Stage -> IO a -> IO a
debugSingle DebugCode Input x    = debugIt Input x
debugSingle DebugCode Output x   = debugIt Output x
debugSingle DebugCode _ x        = x
debugSingle DebugTrees Parser x  = debugIt Parser x
debugSingle DebugTrees Schema x  = debugIt Schema x
debugSingle DebugTrees _ x       = x
debugSingle DebugLexer Lexer x   = debugIt Lexer x
debugSingle DebugLexer _ x       = x
debugSingle DebugParser Parser x = debugIt Parser x
debugSingle DebugParser _ x      = x
debugSingle DebugAsm Output x    = debugIt Output x
debugSingle DebugAsm _ x         = x
debugSingle DebugState State x   = debugIt State x
debugSingle DebugState _ x       = x
debugSingle DebugSchema Schema x = debugIt Schema x
debugSingle DebugSchema _ x      = x
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
        DebugPrint.printDebug stage y
        x
