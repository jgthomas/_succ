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
import Tokens    (Keyword (..), OpTok (..), Token (..))


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
        case c of
             '(' -> updateState OpenParen cs
             ')' -> updateState CloseParen cs
             '{' -> updateState OpenBrace cs
             '}' -> updateState CloseBrace cs
             ';' -> updateState SemiColon cs
             ':' -> updateState Colon cs
             '?' -> updateState QuestMark cs
             ',' -> updateState Comma cs
             _   -> throwError $ LexerError (UnexpectedInput [c])


identifier :: String -> LexerState [Token]
identifier [] = throwError ImpossibleError
identifier (c:cs) =
        let (str, cs') = span isValidInIdentifier cs
            in
        case c:str of
             "int"      -> updateState (Keyword Int) cs'
             "return"   -> updateState (Keyword Return) cs'
             "if"       -> updateState (Keyword If) cs'
             "else"     -> updateState (Keyword Else) cs'
             "for"      -> updateState (Keyword For) cs'
             "while"    -> updateState (Keyword While) cs'
             "do"       -> updateState (Keyword Do) cs'
             "break"    -> updateState (Keyword Break) cs'
             "continue" -> updateState (Keyword Continue) cs'
             _          -> updateState (Ident (c:str)) cs'


number :: String -> LexerState [Token]
number [] = throwError ImpossibleError
number (c:cs) =
        let (digs, cs') = span isDigit cs
            tok         = ConstInt (read (c:digs))
            in
        updateState tok cs'


twoCharOperator :: String -> LexerState [Token]
twoCharOperator []  = throwError ImpossibleError
twoCharOperator [_] = throwError ImpossibleError
twoCharOperator (c:n:cs) =
        case c:[n] of
             "||" -> updateState (OpTok PipePipe) cs
             "&&" -> updateState (OpTok AmpAmp) cs
             ">=" -> updateState (OpTok RightArrowEqual) cs
             "<=" -> updateState (OpTok LeftArrowEqual) cs
             "==" -> updateState (OpTok EqualEqual) cs
             "!=" -> updateState (OpTok BangEqual) cs
             "+=" -> updateState (OpTok PlusEqual) cs
             "-=" -> updateState (OpTok MinusEqual) cs
             "*=" -> updateState (OpTok AsteriskEqual) cs
             "/=" -> updateState (OpTok BackslashEqual) cs
             "%=" -> updateState (OpTok PercentEqual) cs
             _    -> throwError $ LexerError (UnexpectedInput (c:[n]))


operator :: String -> LexerState [Token]
operator [] = throwError ImpossibleError
operator (c:cs) =
        case c of
             '+' -> updateState (OpTok PlusSign) cs
             '-' -> updateState (OpTok MinusSign) cs
             '*' -> updateState (OpTok Asterisk) cs
             '%' -> updateState (OpTok Percent) cs
             '/' -> updateState (OpTok BackSlash) cs
             '~' -> updateState (OpTok Tilde) cs
             '!' -> updateState (OpTok Bang) cs
             '>' -> updateState (OpTok RightArrow) cs
             '<' -> updateState (OpTok LeftArrow) cs
             '=' -> updateState (OpTok EqualSign) cs
             '&' -> updateState (OpTok Ampersand) cs
             _   -> throwError $ LexerError (UnexpectedInput [c])


updateState :: Token -> String -> LexerState [Token]
updateState tok input = do
        lexOut <- getState
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
