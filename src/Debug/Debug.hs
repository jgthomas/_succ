{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug.Debug (debug) where


import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor,
                           pPrint, pPrintOpt)

import SuccTokens         (Debug (..), Stage (..))


-- | Print debugging output
debug :: Show a => Debug -> Stage -> IO a -> IO a
debug DebugOff _ x    = x
debug DebugOn stage x = debugIt stage x


debugIt :: Show a => Stage -> IO a -> IO a
debugIt stage x = do
        y <- x
        case stage of
             Input  -> debugString inputTitle y
             Lexer  -> debugDataTypeSimple lexTitle y
             Parser -> debugDataType parTitle y
             State  -> debugDataType stateTitle y
             Output -> debugString outTitle y
        x
        where
                inputTitle = "C CODE"
                lexTitle   = "LEXED TOKENS"
                parTitle   = "ABSTRACT SYNTAX TREE"
                stateTitle = "SYMBOL TABLE"
                outTitle   = "ASSEMBLY CODE"


debugString :: Show a => String -> a -> IO ()
debugString title content = do
        putStrLn title
        pPrintOpt options content
        putStr "\n"
        where
                options = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 0}


debugDataType :: Show a => String -> a -> IO ()
debugDataType title dat = do
        putStrLn title
        pPrint dat
        putStr "\n"


debugDataTypeSimple :: Show a => String -> a -> IO ()
debugDataTypeSimple title dat = do
        putStrLn title
        print dat
        putStr "\n"
