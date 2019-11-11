
module Lexer (tokenize) where


import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isDigit, isAlpha, isSpace)

import Tokens (Operator(..), Keyword(..), Token(..))
import Error  (CompilerError(LexerError), LexerError(..), CompilerM)


type LexerState = State [Token]


tokenize :: String -> Either CompilerError [Token]
tokenize input = evalState (runExceptT $ lexer input) []


lexer :: String -> CompilerM LexerState [Token]
lexer []    = throwE (LexerError EmptyInput)
lexer input = lexInput input


lexInput :: String -> CompilerM LexerState [Token]
lexInput [] = do
        lexOut <- get
        return . reverse $ lexOut
lexInput input@(c:cs) = do
        lexOut <- get
        let (tok, input') = getToken input
        case tok of
             TokUnrecognised -> throwE (LexerError (BadToken [c]))
             TokNull         -> throwE (LexerError EmptyInput)
             TokSpace        -> put lexOut
             _               -> put (tok:lexOut)
        lexInput input'


getToken :: String -> (Token, String)
getToken [] = (TokNull, [])
getToken (c:cs)
    | c `elem` separators = separator c cs
    | isTwoCharOp c cs    = twoCharOperator c cs
    | c `elem` opSymbols  = (TokOp (operator c), cs)
    | identifierStart c   = identifier c cs
    | isDigit c           = number c cs
    | isSpace c           = (TokSpace, cs)
    | otherwise           = (TokUnrecognised, cs)



separator :: Char -> String -> (Token, String)
separator c cs
    | c == '('           = (TokOpenParen, cs)
    | c == ')'           = (TokCloseParen, cs)
    | c == '{'           = (TokOpenBrace, cs)
    | c == '}'           = (TokCloseBrace, cs)
    | c == ';'           = (TokSemiColon, cs)
    | c == ':'           = (TokColon, cs)
    | c == '?'           = (TokQuestMark,  cs)
    | c == ','           = (TokComma, cs)



identifier :: Char -> String -> (Token, String)
identifier c cs =
    let (str, cs') = span isValidInIdentifier cs
        in
    case c:str of
         "int"      -> (TokKeyword Int, cs')
         "return"   -> (TokKeyword Return, cs')
         "if"       -> (TokKeyword If, cs')
         "else"     -> (TokKeyword Else, cs')
         "for"      -> (TokKeyword For, cs')
         "while"    -> (TokKeyword While, cs')
         "do"       -> (TokKeyword Do, cs')
         "break"    -> (TokKeyword Break, cs')
         "continue" -> (TokKeyword Continue, cs')
         _          -> (TokIdent (c:str), cs')


number :: Char -> String -> (Token, String)
number c cs =
    let (digs, cs') = span isDigit cs
        in
    (TokConstInt (read (c:digs)), cs')


twoCharOperator :: Char -> String -> (Token, String)
twoCharOperator c (n:cs) =
        case c:[n] of
             "||" -> (TokOp LogicalOR, cs)
             "&&" -> (TokOp LogicalAND, cs)
             ">=" -> (TokOp GreaterThanOrEqual, cs)
             "<=" -> (TokOp LessThanOrEqual, cs)
             "==" -> (TokOp Equal, cs)
             "!=" -> (TokOp NotEqual, cs)
             "+=" -> (TokOp PlusAssign, cs)
             "-=" -> (TokOp MinusAssign, cs)
             "*=" -> (TokOp MultiplyAssign, cs)
             "/=" -> (TokOp DivideAssign, cs)
             "%=" -> (TokOp ModuloAssign, cs)


isTwoCharOp :: Char -> String -> Bool
isTwoCharOp c [] = False
isTwoCharOp c cs = elem c opSymbols
                   && elem (head cs) secondOpSymbols


operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Multiply
           | c == '%' = Modulo
           | c == '/' = Divide
           | c == '~' = BitwiseCompl
           | c == '!' = LogicNegation
           | c == '>' = GreaterThan
           | c == '<' = LessThan
           | c == '=' = Assign
           | c == '&' = Ampersand


separators :: String
separators = "(){};:,?"


opSymbols :: String
opSymbols = "+-*/~!|&<>=%"


secondOpSymbols :: String
secondOpSymbols = "=|&"


identifierStart :: Char -> Bool
identifierStart c = isAlpha c || c == '_'


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identifierStart c || isDigit c
