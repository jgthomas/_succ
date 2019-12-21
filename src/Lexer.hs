{-|
Module       : Lexer
Description  : Tokenizes an input string

Processes a string representing a program written in C into
a list of tokens.
-}
{-# LANGUAGE MultiWayIf #-}

module Lexer (tokenize) where


import Data.Char (isAlpha, isDigit, isSpace)

import Error     (CompilerError (ImpossibleError, LexerError), LexerError (..))
import SuccState (SuccStateM, getState, putState, runSuccState, throwError)
import Tokens    (Keyword (..), Operator (..), Token (..))


type LexerState = SuccStateM [Token]

startState :: [Token]
startState = []


-- | Convert a string representing a C program to a list of tokens
tokenize :: String -> Either CompilerError [Token]
tokenize input = runSuccState lexer input startState


lexer :: String -> LexerState [Token]
lexer []    = throwError (LexerError EmptyInput)
lexer input = lexInput input


lexInput :: String -> LexerState [Token]
lexInput [] = do
        lexOut <- getState
        pure . reverse $ lexOut
lexInput input@(c:cs) =
        if | isSeparator c    -> separator input
           | isTwoCharOp c cs -> twoCharOperator input
           | isOpSymbol c     -> operator input
           | identStart c     -> identifier input
           | isDigit c        -> number input
           | isSpace c        -> lexInput cs
           | otherwise        -> throwError (LexerError (BadInput [c]))


separator :: String -> LexerState [Token]
separator [] = throwError ImpossibleError
separator (c:cs) =
        let tok | c == '('  = OpenParen
                | c == ')'  = CloseParen
                | c == '{'  = OpenBrace
                | c == '}'  = CloseBrace
                | c == ';'  = SemiColon
                | c == ':'  = Colon
                | c == '?'  = QuestMark
                | c == ','  = Comma
                | otherwise = Wut
            in
        updateLexerState tok cs


identifier :: String -> LexerState [Token]
identifier [] = throwError ImpossibleError
identifier (c:cs) =
        let (str, cs') = span isValidInIdentifier cs
            tok | kwd == "int"      = Keyword Int
                | kwd == "return"   = Keyword Return
                | kwd == "if"       = Keyword If
                | kwd == "else"     = Keyword Else
                | kwd == "for"      = Keyword For
                | kwd == "while"    = Keyword While
                | kwd == "do"       = Keyword Do
                | kwd == "break"    = Keyword Break
                | kwd == "continue" = Keyword Continue
                | otherwise         = Ident (c:str)
                where kwd = c:str
            in
        updateLexerState tok cs'


number :: String -> LexerState [Token]
number [] = throwError ImpossibleError
number (c:cs) =
        let (digs, cs') = span isDigit cs
            tok         = ConstInt (read (c:digs))
            in
        updateLexerState tok cs'


twoCharOperator :: String -> LexerState [Token]
twoCharOperator []  = throwError ImpossibleError
twoCharOperator [_] = throwError ImpossibleError
twoCharOperator (c:n:cs) =
        let tok | op == "||" = Op PipePipe
                | op == "&&" = Op AmpAmp
                | op == ">=" = Op GreaterThanOrEqual
                | op == "<=" = Op LessThanOrEqual
                | op == "==" = Op Equal
                | op == "!=" = Op NotEqual
                | op == "+=" = Op PlusAssign
                | op == "-=" = Op MinusAssign
                | op == "*=" = Op MultiplyAssign
                | op == "/=" = Op DivideAssign
                | op == "%=" = Op ModuloAssign
                | otherwise  = Wut
                where op = c:[n]
            in
        updateLexerState tok cs


operator :: String -> LexerState [Token]
operator [] = throwError ImpossibleError
operator (c:cs) =
        let tok | c == '+'  = Op PlusSign
                | c == '-'  = Op MinusSign
                | c == '*'  = Op Asterisk
                | c == '%'  = Op Percent
                | c == '/'  = Op BackSlash
                | c == '~'  = Op Tilde
                | c == '!'  = Op Bang
                | c == '>'  = Op GreaterThan
                | c == '<'  = Op LessThan
                | c == '='  = Op Assign
                | c == '&'  = Ampersand
                | otherwise = Wut
            in
        updateLexerState tok cs


updateLexerState :: Token -> String -> LexerState [Token]
updateLexerState tok input = do
        lexOut <- getState
        case tok of
             Wut -> throwError ImpossibleError
             _      -> do
                     putState (tok:lexOut)
                     lexInput input


isSeparator :: Char -> Bool
isSeparator c = c `elem` separators


isOpSymbol :: Char -> Bool
isOpSymbol c = c `elem` opSymbols


isSecondOpSymbol :: Char -> Bool
isSecondOpSymbol c = c `elem` secondOpSymbols


isTwoCharOp :: Char -> String -> Bool
isTwoCharOp _ []    = False
isTwoCharOp c (x:_) = isOpSymbol c && isSecondOpSymbol x


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identStart c || isDigit c


opSymbols :: String
opSymbols = "+-*/~!|&<>=%"


separators :: String
separators = "(){};:,?"


secondOpSymbols :: String
secondOpSymbols = "=|&"


identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'
