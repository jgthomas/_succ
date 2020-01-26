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
import LexDat        (LexDat (..))


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                deriving (Eq)


-- | Print error message with relevant section of code
printError :: String -> CompilerError -> IO ()
printError input err = do
        printSource range input
        putStrLn errMsg
        where
                (errMsg, range) = errorMsg err


printSource :: PrintRange -> String -> IO ()
printSource All input         = putStrLn input
printSource (Range n m) input = printSourceLineRange input n m
printSource (Exact n) input   = printSourceLine (toLineMap input) n
printSource None _            = pure ()


printSourceLineRange :: String -> Int -> Int -> IO ()
printSourceLineRange input n m =
        foldr (>>) (pure ()) $ printSourceLine (toLineMap input) <$> [n..m]


printSourceLine :: M.Map Int String -> Int -> IO ()
printSourceLine lineMap n =
        unless (isNothing sourceLine) $
            putStrLn $ fromMaybe "" sourceLine
        where
                sourceLine = M.lookup n lineMap


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
parserErrorMsg err@(TreeError _)     = (show err, All)
parserErrorMsg err@(LexDataError []) = (show err, All)
parserErrorMsg (LexDataError (d:_))  = (msg, Exact $ line d)
        where msg = "Unexpected input: "
                    ++ show (tok d)
                    ++ buildLineMsg (line d)


generatorErrorMsg :: GeneratorError -> (String, PrintRange)
generatorErrorMsg err = (show err, All)


syntaxErrorMsg :: SyntaxError -> (String, PrintRange)
syntaxErrorMsg (MissingToken t d) = (msg, Exact $ line d)
        where msg = "Unexpected character: "
                    ++ show (tok d)
                    ++ " Expected: "
                    ++ show t
                    ++ buildLineMsg (line d)
syntaxErrorMsg err = (show err, All)


typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg err = (show err, All)


impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = ("Something unexpected went wrong, you are on your own!", None)


toLineMap :: String -> M.Map Int String
toLineMap input = M.fromList $ zip [1..] $ lines input


buildLineMsg :: Int -> String
buildLineMsg n = " : Line " ++ show n
