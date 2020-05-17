{-|
Module       : DebugPrint
Description  : Print debug output

Formats and prints the output of the internal debugger.
-}
module Debug.DebugPrint
        (printString,
         printDataType,
         printDataTypeSimple
        ) where


import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor,
                           pPrint, pPrintOpt)


-- | Prints a source file string
printString :: Show a => String -> a -> IO ()
printString title content = do
        putStrLn title
        pPrintOpt options content
        putStr "\n"
        where
                options = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 0}


-- | Prints a prettily formatted data structure
printDataType :: Show a => String -> a -> IO ()
printDataType title dat = do
        putStrLn title
        pPrint dat
        putStr "\n"


-- | Prints a simply formatted data structure
printDataTypeSimple :: Show a => String -> a -> IO ()
printDataTypeSimple title dat = do
        putStrLn title
        print dat
        putStr "\n"
