{-|
Module       : PrintError
Description  : Output error messages

Create and format error messages with associated code sections
-}
module PrintError (printError) where


import Data.Map as M (Map, elems, lookup)

import Error


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                deriving (Eq)


-- | Print error message with relevant section of code
printError :: M.Map Int String -> CompilerError -> IO ()
printError lineMap err = do
        let (errMsg, range) = errorMsg err
        printSource range lineMap
        putStrLn errMsg


printSource :: PrintRange -> M.Map Int String -> IO ()
printSource All lineMap         = printAllSourceLines lineMap
printSource (Exact n) lineMap   = printSourceLine lineMap n
printSource (Range n m) lineMap = printSourceLineRange lineMap n m
printSource None _              = pure ()


printAllSourceLines :: M.Map Int String -> IO ()
printAllSourceLines lineMap = putStrLn . unlines . M.elems $ lineMap


printSourceLine :: M.Map Int String -> Int -> IO ()
printSourceLine lineMap n = do
        let line = M.lookup n lineMap
        case line of
             Just l  -> putStrLn l
             Nothing -> pure ()


printSourceLineRange :: M.Map Int String -> Int -> Int -> IO ()
printSourceLineRange lineMap n m = printRange $ printSourceLine lineMap <$> [n..m]


printRange :: [IO ()] -> IO ()
printRange = foldr (>>) (pure ())


errorMsg :: CompilerError -> (String, PrintRange)
errorMsg (LexerError err)     = lexerErrorMsg err
errorMsg (ParserError err)    = parserErrorMsg err
errorMsg (GeneratorError err) = generatorErrorMsg err
errorMsg (SyntaxError err)    = syntaxErrorMsg err
errorMsg (TypeError err)      = typeErrorMsg err
errorMsg ImpossibleError      = impossibleErrorMsg


lexerErrorMsg :: LexerError -> (String, PrintRange)
lexerErrorMsg err =
        case err of
             UnexpectedInput str -> (lexerUnexpectedMsg str, All)
             EmptyInput          -> ("Empty input file", None)


lexerUnexpectedMsg :: String -> String
lexerUnexpectedMsg str =
        case str of
             []    -> msg ++ "Empty file"
             [c]   -> msg ++ "'" ++ [c] ++ "'"
             (c:_) -> msg ++ "'" ++ [c] ++ "'"
        where msg = "Unexpected input: "


parserErrorMsg :: ParserError -> (String, PrintRange)
parserErrorMsg err = (show err, All)


generatorErrorMsg :: GeneratorError -> (String, PrintRange)
generatorErrorMsg err = (show err, All)


syntaxErrorMsg :: SyntaxError -> (String, PrintRange)
syntaxErrorMsg err = (show err, All)


typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg err = (show err, All)


impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = ("Something unexpected went wrong, you are on your own!", None)
