{-|
Module       : PrintError
Description  : Output error messages

Create and format error messages with associated code sections
-}
module PrintError (printError) where


import Control.Monad (unless)
import Data.Map      as M (Map, fromList, lookup)
import Data.Maybe    (fromMaybe, isNothing)

import Error


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                deriving (Eq)


-- | Print error message with relevant section of code
printError :: String -> CompilerError -> IO ()
printError input err = do
        printSourceLines range (toLineMap input)
        putStrLn errMsg
        where
                (errMsg, range) = errorMsg err
                printSourceLines = printSource input


printSource :: String -> PrintRange -> M.Map Int String -> IO ()
printSource input All _           = putStrLn input
printSource _ (Exact n) lineMap   = printSourceLine lineMap n
printSource _ (Range n m) lineMap = printSourceLineRange lineMap n m
printSource _ None _              = pure ()


printSourceLine :: M.Map Int String -> Int -> IO ()
printSourceLine lineMap n =
        unless (isNothing line) $
            putStrLn $ fromMaybe "" line
        where
                line = M.lookup n lineMap


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


toLineMap :: String -> M.Map Int String
toLineMap input = M.fromList $ zip [1..] $ lines input
