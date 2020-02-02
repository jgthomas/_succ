{-|
Module       : PrintError
Description  : Output error messages

Create and format error messages with associated code sections
-}
module PrintError (printError) where


import Control.Monad (unless)
import Data.Map      as M (Map, fromList, lookup)
import Data.Maybe    (fromMaybe, isNothing)

import AST           (NodeDat (..), Tree (..))
import Error
import LexDat        (LexDat (..))
import Tokens        (Token)
import Type          (Type (..))


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                | From Int
                | Until Int
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
stateErrorMsg err = (show err, All)


checkerErrorMsg :: CheckerError -> (String, PrintRange)
checkerErrorMsg err = (show err, All)


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
scopeErrorMsg (DoubleDefinedNode (FunctionNode _ name _ _ dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Identifier '" ++ name ++ "' already defined"
scopeErrorMsg (UnexpectedNode node@BreakNode{})    = unexpectedNodeErrMsg node
scopeErrorMsg (UnexpectedNode node@ContinueNode{}) = unexpectedNodeErrMsg node
scopeErrorMsg err = (show err, All)


unexpectedNodeErrMsg :: Tree -> (String, PrintRange)
unexpectedNodeErrMsg (BreakNode dat) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'break' outside loop context"
unexpectedNodeErrMsg (ContinueNode dat) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'continue' outside loop context"
unexpectedNodeErrMsg _ = undefined


typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg (TypeMismatch a b (FunctionNode _ name _ _ dat)) =
        (msg, Exact (startLine dat))
        where msg = buildLineMsg (startLine dat)
                    ++ "Parameter type mismatch between declarations of '" ++ name
                    ++ "' was '" ++ typeString a
                    ++ "' now '" ++ typeString b ++ "'"
typeErrorMsg (TypeMismatch a b (AssignmentNode name _ _ dat)) =
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
