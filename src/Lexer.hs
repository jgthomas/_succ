{-# LANGUAGE MultiWayIf #-}

module Lexer (tokenize) where


import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isDigit, isAlpha, isSpace)

import Tokens (Operator(..),
               Keyword(..),
               Token(..))
import Error  (CompilerError(LexerError, ImpossibleError),
               LexerError(..))
import Types  (CompilerM)


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
lexInput input@(c:cs) =
        if | isSeparator c     -> separator input
           | isTwoCharOp c cs  -> twoCharOperator input
           | isOpSymbol c      -> operator input
           | identifierStart c -> identifier input
           | isDigit c         -> number input
           | isSpace c         -> lexInput cs
           | otherwise         -> throwE (LexerError (BadToken [c]))


separator :: String -> CompilerM LexerState [Token]
separator [] = throwE ImpossibleError
separator (c:cs) =
        let tok | c == '('  = TokOpenParen
                | c == ')'  = TokCloseParen
                | c == '{'  = TokOpenBrace
                | c == '}'  = TokCloseBrace
                | c == ';'  = TokSemiColon
                | c == ':'  = TokColon
                | c == '?'  = TokQuestMark
                | c == ','  = TokComma
                | otherwise = TokWut
            in
        updateLexerState tok cs


identifier :: String -> CompilerM LexerState [Token]
identifier [] = throwE ImpossibleError
identifier (c:cs) =
        let (str, cs') = span isValidInIdentifier cs
            tok | kwd == "int"      = TokKeyword Int
                | kwd == "return"   = TokKeyword Return
                | kwd == "if"       = TokKeyword If
                | kwd == "else"     = TokKeyword Else
                | kwd == "for"      = TokKeyword For
                | kwd == "while"    = TokKeyword While
                | kwd == "do"       = TokKeyword Do
                | kwd == "break"    = TokKeyword Break
                | kwd == "continue" = TokKeyword Continue
                | otherwise         = TokIdent (c:str)
                where kwd = c:str
            in
        updateLexerState tok cs'


number :: String -> CompilerM LexerState [Token]
number [] = throwE ImpossibleError
number (c:cs) =
        let (digs, cs') = span isDigit cs
            tok         = (TokConstInt (read (c:digs)))
            in
        updateLexerState tok cs'


twoCharOperator :: String -> CompilerM LexerState [Token]
twoCharOperator []  = throwE ImpossibleError
twoCharOperator [_] = throwE ImpossibleError
twoCharOperator (c:n:cs) =
        let tok | op == "||" = TokOp LogicalOR
                | op == "&&" = TokOp LogicalAND
                | op == ">=" = TokOp GreaterThanOrEqual
                | op == "<=" = TokOp LessThanOrEqual
                | op == "==" = TokOp Equal
                | op == "!=" = TokOp NotEqual
                | op == "+=" = TokOp PlusAssign
                | op == "-=" = TokOp MinusAssign
                | op == "*=" = TokOp MultiplyAssign
                | op == "/=" = TokOp DivideAssign
                | op == "%=" = TokOp ModuloAssign
                | otherwise  = TokWut
                where op = c:[n]
            in
        updateLexerState tok cs


operator :: String -> CompilerM LexerState [Token]
operator [] = throwE ImpossibleError
operator (c:cs) =
        let tok | c == '+'  = TokOp Plus
                | c == '-'  = TokOp Minus
                | c == '*'  = TokOp Multiply
                | c == '%'  = TokOp Modulo
                | c == '/'  = TokOp Divide
                | c == '~'  = TokOp BitwiseCompl
                | c == '!'  = TokOp LogicNegation
                | c == '>'  = TokOp GreaterThan
                | c == '<'  = TokOp LessThan
                | c == '='  = TokOp Assign
                | c == '&'  = TokOp Ampersand
                | otherwise = TokWut
            in
        updateLexerState tok cs


updateLexerState :: Token -> String -> CompilerM LexerState [Token]
updateLexerState tok input = do
        lexOut <- get
        case tok of
             TokWut -> throwE ImpossibleError
             _      -> do
                     put (tok:lexOut)
                     lexInput input


isSeparator :: Char -> Bool
isSeparator c = c `elem` separators


isOpSymbol :: Char -> Bool
isOpSymbol c = c `elem` opSymbols


isSecondOpSymbol :: Char -> Bool
isSecondOpSymbol c = c `elem` secondOpSymbols


isTwoCharOp :: Char -> String -> Bool
isTwoCharOp _ [] = False
isTwoCharOp c (x:_) = isOpSymbol c && isSecondOpSymbol x


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identifierStart c || isDigit c


opSymbols :: String
opSymbols = "+-*/~!|&<>=%"


separators :: String
separators = "(){};:,?"


secondOpSymbols :: String
secondOpSymbols = "=|&"


identifierStart :: Char -> Bool
identifierStart c = isAlpha c || c == '_'
