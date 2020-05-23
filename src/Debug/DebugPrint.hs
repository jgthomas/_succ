{-|
Module       : DebugPrint
Description  : Print debug output

Formats and prints the output of the internal debugger.
-}
module Debug.DebugPrint (printDebug) where


import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor,
                           pPrint, pPrintOpt)

import Types.SuccTokens   (Stage (..))


-- | Print debugging information for current stage of compilation
printDebug :: Show a => Stage -> a -> IO ()
printDebug stage content =
        case stage of
             Input  -> printInputDebug content
             Lexer  -> printLexerDebug content
             Parser -> printParserDebug content
             State  -> printStateDebug content
             Output -> printOutputDebug content


printInputDebug :: Show a => a -> IO ()
printInputDebug content = do
        printTitleAndMessage title message
        printCode content
        where
                title = "C CODE"
                message = "Source code being compiled"


printLexerDebug :: Show a => a -> IO ()
printLexerDebug content = do
        printTitleAndMessage title message
        printDataSimple content
        where
                title = "LEXED TOKENS"
                message = "List of tokens lexed from the source code"


printParserDebug :: Show a => a -> IO ()
printParserDebug content = do
        printTitleAndMessage title message
        printData content
        where
                title = "ABSTRACT SYNTAX TREE"
                message = "Syntax tree parsed from the lexed tokens"


printStateDebug :: Show a => a -> IO ()
printStateDebug content = do
        printTitleAndMessage title message
        printData content
        where
                title = "SYMBOL TABLE"
                message = "State used by code generator to track place in syntax tree"


printOutputDebug :: Show a => a -> IO ()
printOutputDebug content = do
        printTitleAndMessage title message
        printCode content
        where
                title = "ASSEMBLY CODE"
                message = "Final assembly code produced by the compiler"


printTitleAndMessage :: String -> String -> IO ()
printTitleAndMessage title msg = do
        putStr "\n"
        putStrLn title
        putStr "\n"
        putStrLn msg
        putStr "\n"


printCode :: Show a => a -> IO ()
printCode content = do
        pPrintOpt options content
        putStr "\n"
        where
                options = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 0}


printDataSimple :: Show a => a -> IO ()
printDataSimple content = do
        print content
        putStr "\n"


printData :: Show a => a -> IO ()
printData content = do
        pPrint content
        putStr "\n"
