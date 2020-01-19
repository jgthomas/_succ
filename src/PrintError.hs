
module PrintError (printError) where


import Error


printError :: CompilerError -> IO ()
printError (LexerError err)     = printLexerError err
printError (ParserError err)    = printParserError err
printError (GeneratorError err) = printGeneratorError err
printError (SyntaxError err)    = printSyntaxError err
printError (TypeError err)      = printTypeError err
printError ImpossibleError      = printImpossibleError


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
