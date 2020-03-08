{-|
Module       : PrintError
Description  : Output error messages

Create and format error messages with associated code sections
-}
module PrintError (printError) where


import Control.Monad    (unless)
import Data.Map         as M (Map, fromList, lookup)
import Data.Maybe       (fromMaybe, isNothing)

import AST              (NodeDat (..), Tree (..))
import Error
import LexTab           (LexDat (..))
import PrintErrorSyntax (syntaxErrorMsg)
import PrintErrorTokens (PrintRange (..), buildLineMsg, buildTokMsg)
import SuccTokens       (Debug (..))
import Type             (Type (..))


-- | Print error message with relevant section of code
printError :: Debug -> String -> CompilerError -> IO ()
printError DebugOn input err  = printDebugError input err
printError DebugOff input err = printUserError input err


printUserError :: String -> CompilerError -> IO ()
printUserError input err = do
        formatSourcePrint range input
        putStrLn errMsg
        where (errMsg, range) = errorMsg err


printDebugError :: String -> CompilerError -> IO ()
printDebugError input err = do
        formatSourcePrint All input
        print err


formatSourcePrint :: PrintRange -> String -> IO ()
formatSourcePrint range input = do
        putStr "\n"
        printSource range input
        putStr "\n"


printSource :: PrintRange -> String -> IO ()
printSource All input         = printSourceLineRange input 1 (lineCount input)
printSource (Range n m) input = printSourceLineRange input n m
printSource (Exact n) input   = printSourceLine (toLineMap input) n
printSource (From n) input    = printSourceLineRange input n (lineCount input)
printSource (Until n) input   = printSourceLineRange input 1 n
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
errorMsg (LexerError err)   = lexerErrorMsg err
errorMsg (ParserError err)  = parserErrorMsg err
errorMsg (StateError err)   = stateErrorMsg err
errorMsg (CheckerError err) = checkerErrorMsg err
errorMsg (SyntaxError err)  = syntaxErrorMsg err
errorMsg (ScopeError err)   = scopeErrorMsg err
errorMsg (TypeError err)    = typeErrorMsg err
errorMsg (FatalError err)   = fatalErrorMsg err
errorMsg ImpossibleError    = impossibleErrorMsg


lexerErrorMsg :: LexerError -> (String, PrintRange)
lexerErrorMsg (UnexpectedInput s) = (msg, All)
        where msg = lexerUnexpectedMsg s
lexerErrorMsg EmptyInput = (msg, None)
        where msg = "Empty input file"


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
parserErrorMsg (LexDataError [d])  = (msg, Exact $ line d)
        where msg = buildLineMsg (line d)
                    ++ "Unexpected input "
                    ++ buildTokMsg (tok d)
parserErrorMsg (LexDataError (d:_)) = (msg, From $ line d)
        where msg = buildLineMsg (line d)
                    ++ "Unexpected input starting at '"
                    ++ buildTokMsg (tok d) ++ "'"


stateErrorMsg :: StateError -> (String, PrintRange)
stateErrorMsg (NoStateFound name) = (msg, None)
        where msg = "Unable to locate any state for '" ++ name
                    ++ "' compilation terminated"
stateErrorMsg (UndefinedScope name scope) = (msg, None)
        where msg = "Unable to locate state for scope '" ++ show scope
                    ++ "' in '" ++ name ++ "' compilation terminated"


checkerErrorMsg :: CheckerError -> (String, PrintRange)
checkerErrorMsg err = (show err, All)


scopeErrorMsg :: ScopeError -> (String, PrintRange)
scopeErrorMsg (DoubleDefinedNode (FunctionNode _ name _ _ dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Identifier '" ++ name ++ "' already defined"
scopeErrorMsg (UnexpectedNode (BreakNode dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'break' outside loop context"
scopeErrorMsg (UnexpectedNode (ContinueNode dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'continue' outside loop context"
scopeErrorMsg (UndeclaredNode (FuncCallNode name _ dat)) =
              (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Calling undeclared function '" ++ name ++ "'"
scopeErrorMsg err = (show err, All)


typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg (TypeMismatch a b (FunctionNode _ name _ _ dat)) =
        (msg, Exact (startLine dat))
        where msg = buildLineMsg (startLine dat)
                    ++ "Parameter type mismatch between declarations of '" ++ name
                    ++ "' was '" ++ typeString a
                    ++ "' now '" ++ typeString b ++ "'"
typeErrorMsg (TypeMismatch a b (AssignmentNode (VarNode name _) _ _ dat)) =
        (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Type mismatch for '" ++ name
                    ++ "' between declaration '" ++ typeString a
                    ++ "' and assignment '" ++ typeString b
typeErrorMsg err = (show err, All)


typeString :: [Type] -> String
typeString ts = unwords . map show $ ts


fatalErrorMsg :: FatalError -> (String, PrintRange)
fatalErrorMsg err@(LexerBug input) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ input ++ fatalErrorMsgOutro
fatalErrorMsg err@(ParserBug lexData) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show lexData ++ fatalErrorMsgOutro
fatalErrorMsg err@(CheckerBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro
fatalErrorMsg err@(GeneratorBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro


fatalErrorMsgIntro :: FatalError -> String
fatalErrorMsgIntro err = "There is a bug in the " ++ component
                         ++ ", this is the state I was working with: "
        where component = fatalComponent err


fatalComponent :: FatalError -> String
fatalComponent err =
        case err of
             LexerBug{}     -> "lexer"
             ParserBug{}    -> "parser"
             CheckerBug{}   -> "syntax tree checker"
             GeneratorBug{} -> "code generator"


fatalErrorMsgOutro :: String
fatalErrorMsgOutro = " good luck!"


impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = (msg, None)
        where msg = "Something unexpected went wrong, you are on your own!"


toLineMap :: String -> M.Map Int String
toLineMap input = M.fromList $ zip [1..] $ lines input


lineCount :: String -> Int
lineCount input = length $ filter (== '\n') input
