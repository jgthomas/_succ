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
        putStrLn . errorMsg $ err



errorMsg :: CompilerError -> String
errorMsg (LexerError err)     = lexerErrorMsg err
errorMsg (ParserError err)    = parserErrorMsg err
errorMsg (GeneratorError err) = generatorErrorMsg err
errorMsg (SyntaxError err)    = syntaxErrorMsg err
errorMsg (TypeError err)      = typeErrorMsg err
errorMsg ImpossibleError      = impossibleErrorMsg


lexerErrorMsg :: LexerError -> String
lexerErrorMsg err =
        case err of
             UnexpectedInput str -> lexerUnexpectedMsg str
             EmptyInput          -> "Empty input file"


lexerUnexpectedMsg :: String -> String
lexerUnexpectedMsg str =
        case str of
             []    -> msg ++ "Empty file"
             [c]   -> msg ++ "'" ++ [c] ++ "'"
             (c:_) -> msg ++ "'" ++ [c] ++ "'"
        where msg = "Unexpected input: "


parserErrorMsg :: ParserError -> String
parserErrorMsg err = show err


generatorErrorMsg :: GeneratorError -> String
generatorErrorMsg err = show err


syntaxErrorMsg :: SyntaxError -> String
syntaxErrorMsg err = show err


typeErrorMsg :: TypeError -> String
typeErrorMsg err = show err


impossibleErrorMsg :: String
impossibleErrorMsg = "Something unexpected went wrong, you are on your own!"
