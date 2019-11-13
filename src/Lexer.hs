
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
lexInput input = getTokens input


getTokens :: String -> CompilerM LexerState [Token]
getTokens (c:cs) =
        case c of
             c | c `elem` separators -> separator c cs
               | isTwoCharOp c cs    -> twoCharOperator c cs
               | c `elem` opSymbols  -> operator (c:cs)
               | identifierStart c   -> identifier c cs
               | isDigit c           -> number c cs
               | isSpace c           -> lexInput cs
               | otherwise           -> throwE (LexerError (BadToken [c]))


separator :: Char -> String -> CompilerM LexerState [Token]
separator c cs = do
        lexOut <- get
        let tok = case c of
                       '(' -> TokOpenParen
                       ')' -> TokCloseParen
                       '{' -> TokOpenBrace
                       '}' -> TokCloseBrace
                       ';' -> TokSemiColon
                       ':' -> TokColon
                       '?' -> TokQuestMark
                       ',' -> TokComma
            in do
                    put (tok:lexOut)
                    lexInput cs


identifier :: Char -> String -> CompilerM LexerState [Token]
identifier c cs = do
        lexOut <- get
        let (str, cs') = span isValidInIdentifier cs
            tok = case c:str of
                       "int"      -> TokKeyword Int
                       "return"   -> TokKeyword Return
                       "if"       -> TokKeyword If
                       "else"     -> TokKeyword Else
                       "for"      -> TokKeyword For
                       "while"    -> TokKeyword While
                       "do"       -> TokKeyword Do
                       "break"    -> TokKeyword Break
                       "continue" -> TokKeyword Continue
                       _          -> TokIdent (c:str)
            in do
                    put (tok:lexOut)
                    lexInput cs'


number :: Char -> String -> CompilerM LexerState [Token]
number c cs = do
        lexOut <- get
        let (digs, cs') = span isDigit cs
            tok         = (TokConstInt (read (c:digs)))
            in do
                    put (tok:lexOut)
                    lexInput cs'


twoCharOperator :: Char -> String -> CompilerM LexerState [Token]
twoCharOperator c (n:cs) = do
        lexOut <- get
        let tok = case c:[n] of
                       "||" -> TokOp LogicalOR
                       "&&" -> TokOp LogicalAND
                       ">=" -> TokOp GreaterThanOrEqual
                       "<=" -> TokOp LessThanOrEqual
                       "==" -> TokOp Equal
                       "!=" -> TokOp NotEqual
                       "+=" -> TokOp PlusAssign
                       "-=" -> TokOp MinusAssign
                       "*=" -> TokOp MultiplyAssign
                       "/=" -> TokOp DivideAssign
                       "%=" -> TokOp ModuloAssign
                       --_    -> throwE (LexerError (BadToken c:[n]))
            in do
                    put (tok:lexOut)
                    lexInput cs


operator :: String -> CompilerM LexerState [Token]
operator (c:cs) = do
        lexOut <- get
        let tok | c == '+' = TokOp Plus
                | c == '-' = TokOp Minus
                | c == '*' = TokOp Multiply
                | c == '%' = TokOp Modulo
                | c == '/' = TokOp Divide
                | c == '~' = TokOp BitwiseCompl
                | c == '!' = TokOp LogicNegation
                | c == '>' = TokOp GreaterThan
                | c == '<' = TokOp LessThan
                | c == '=' = TokOp Assign
                | c == '&' = TokOp Ampersand
                -- | otherwise = throwE (LexerError (BadToken [c]))
            in do
                    put (tok:lexOut)
                    lexInput cs


isTwoCharOp :: Char -> String -> Bool
isTwoCharOp c [] = False
isTwoCharOp c cs = elem c opSymbols
                   && elem (head cs) secondOpSymbols


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
