{-|
Module       : PrintError
Description  : Output error messages

Create and format error messages with associated code sections
-}
module PrintError (printError) where


import Error


-- | Print error message with relevant section of code
printError :: String -> CompilerError -> IO ()
printError input err = do
        putStrLn input
        printErrorType err


printErrorType :: CompilerError -> IO ()
printErrorType (LexerError err)     = printLexerError err
printErrorType (ParserError err)    = printParserError err
printErrorType (GeneratorError err) = printGeneratorError err
printErrorType (SyntaxError err)    = printSyntaxError err
printErrorType (TypeError err)      = printTypeError err
printErrorType ImpossibleError      = printImpossibleError


printLexerError :: LexerError -> IO ()
printLexerError err =
        case err of
             UnexpectedInput str -> printLexerUnexpected str
             EmptyInput          -> putStr "Empty input file"


printLexerUnexpected :: String -> IO ()
printLexerUnexpected str =
        case str of
             []    -> putStrLn $ msg ++ "Empty file"
             [c]   -> putStrLn $ msg ++ "'" ++ [c] ++ "'"
             (c:_) -> putStrLn $ msg ++ "'" ++ [c] ++ "'"
        where msg = "Unexpected input: "


printParserError :: ParserError -> IO ()
printParserError err = print err


printGeneratorError :: GeneratorError -> IO ()
printGeneratorError err = print err


printSyntaxError :: SyntaxError -> IO ()
printSyntaxError err = print err


printTypeError :: TypeError -> IO ()
printTypeError err = print err


printImpossibleError :: IO ()
printImpossibleError = print "Something went wrong!"
