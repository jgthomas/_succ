{-# LANGUAGE MultiWayIf #-}

module Lexer (tokenize) where


import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isDigit, isAlpha, isSpace)

import Tokens (Operator(..),
               Keyword(..),
               Token(..))
import Error  (CompilerError(LexerError, ImpossibleError),
               LexerError(..),
               CompilerM)


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
           | isTwoCharOp c cs  -> twoCharOperator c cs
           | isOpSymbol c      -> operator (c:cs)
           | identifierStart c -> identifier input
           | isDigit c         -> number input
           | isSpace c         -> lexInput cs
           | otherwise         -> throwE (LexerError (BadToken [c]))


separator :: String -> CompilerM LexerState [Token]
separator (c:cs) =
        let tok | c == '('   = TokOpenParen
                | c == ')'   = TokCloseParen
                | c == '{'   = TokOpenBrace
                | c == '}'   = TokCloseBrace
                | c == ';'   = TokSemiColon
                | c == ':'   = TokColon
                | c == '?'   = TokQuestMark
                | c == ','   = TokComma
                | otherwise  = TokWut
            in
        updateLexerState tok cs


identifier :: String -> CompilerM LexerState [Token]
identifier (c:cs) =
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
            in
        updateLexerState tok cs'


number :: String -> CompilerM LexerState [Token]
number (c:cs) =
        let (digs, cs') = span isDigit cs
            tok         = (TokConstInt (read (c:digs)))
            in
        updateLexerState tok cs'


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
                       _    -> TokUnrecognised
            in case tok of
                    TokUnrecognised -> throwE (LexerError (BadToken (c:[n])))
                    _               -> do
                            put (tok:lexOut)
                            lexInput cs


operator :: String -> CompilerM LexerState [Token]
operator (c:cs) = do
        lexOut <- get
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
                | otherwise = TokUnrecognised
            in case tok of
                    TokUnrecognised -> throwE (LexerError (BadToken [c]))
                    _               -> do
                            put (tok:lexOut)
                            lexInput cs


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
isTwoCharOp c [] = False
isTwoCharOp c (x:cs) = isOpSymbol c && isSecondOpSymbol x


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
