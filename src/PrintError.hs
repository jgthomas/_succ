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
import Tokens        (Token)


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                deriving (Eq)


-- | Print error message with relevant section of code
printError :: String -> CompilerError -> IO ()
printError input err = do
        formatSourcePrint range input
        putStrLn errMsg
        where (errMsg, range) = errorMsg err


formatSourcePrint :: PrintRange -> String -> IO ()
formatSourcePrint range input = do
        putStr "\n"
        printSource range input
        putStr "\n"


printSource :: PrintRange -> String -> IO ()
printSource All input         = printSourceLineRange input 1 (lineCount input)
printSource (Range n m) input = printSourceLineRange input n m
printSource (Exact n) input   = printSourceLine (toLineMap input) n
printSource None _            = pure ()


printSourceLineRange :: String -> Int -> Int -> IO ()
printSourceLineRange input n m =
        foldr (>>) (pure ()) $ printSourceLine (toLineMap input) <$> [n..m]


printSourceLine :: M.Map Int String -> Int -> IO ()
printSourceLine lineMap n =
        unless (isNothing sourceLine) $
            putStrLn $ show n ++ "  |  " ++ fromMaybe "" sourceLine
        where sourceLine = M.lookup n lineMap


errorMsg :: CompilerError -> (String, PrintRange)
errorMsg (LexerError err)     = lexerErrorMsg err
errorMsg (ParserError err)    = parserErrorMsg err
errorMsg (GeneratorError err) = generatorErrorMsg err
errorMsg (SyntaxError err)    = syntaxErrorMsg err
errorMsg (ScopeError err)     = scopeErrorMsg err
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
parserErrorMsg err@(TreeError _) = (show err, All)
parserErrorMsg (LexDataError []) = (msg, None)
        where msg = "Empty input from lexer"
parserErrorMsg (LexDataError (d:_))  = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Unexpected input "
                    ++ buildTokMsg (tok d)


generatorErrorMsg :: GeneratorError -> (String, PrintRange)
generatorErrorMsg err = (show err, All)


syntaxErrorMsg :: SyntaxError -> (String, PrintRange)
syntaxErrorMsg (MissingToken t d) = (msg, mkRange d)
        where msg = unexpectedLexDatMsg d
                    ++ ", Expected "
                    ++ buildTokMsg t
syntaxErrorMsg (BadType d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Invalid type "
                    ++ buildTokMsg (tok d)
syntaxErrorMsg (UnexpectedLexDat d) = (msg, mkRange d)
        where msg = unexpectedLexDatMsg d
syntaxErrorMsg (NonValidIdentifier d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Invalid identifier "
                    ++ buildTokMsg (tok d)
syntaxErrorMsg (MissingKeyword kwd d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Expected keyword " ++ show kwd


scopeErrorMsg :: ScopeError -> (String, PrintRange)
scopeErrorMsg err = (show err, All)


typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg err = (show err, All)


impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = ("Something unexpected went wrong, you are on your own!", None)


toLineMap :: String -> M.Map Int String
toLineMap input = M.fromList $ zip [1..] $ lines input


buildLineMsg :: Int -> String
buildLineMsg n = "Line " ++ show n ++ ": "


buildTokMsg :: Token -> String
buildTokMsg t = "'" ++ show t ++ "'"


lineCount :: String -> Int
lineCount input = length $ filter (== '\n') input


mkRange :: LexDat -> PrintRange
mkRange d = Range (pred . line $ d) (succ . line $ d)


unexpectedLexDatMsg :: LexDat -> String
unexpectedLexDatMsg d =
        buildLineMsg (line d)
        ++ "Unexpected token "
        ++ buildTokMsg (tok d)
