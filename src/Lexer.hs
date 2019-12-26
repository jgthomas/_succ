{-|
Module       : Lexer
Description  : Tokenizes an input string

Processes a string representing a program written in C into
a list of tokens.
-}
module Lexer (tokenize) where


import           Data.Char (isAlpha, isDigit, isSpace)

import           Error     (CompilerError (ImpossibleError, LexerError),
                            LexerError (..))
import           LexState  (LexerState, runLexState, throwError)
import qualified LexState  (getState, putState, startState)
import           Tokens    (Keyword (..), OpTok (..), Token (..))


-- | Convert a string representing a C program to a list of tokens
tokenize :: String -> Either CompilerError [Token]
tokenize input = runLexState lexer input LexState.startState


lexer :: String -> LexerState [Token]
lexer []    = throwError (LexerError EmptyInput)
lexer input = lexInput input


lexInput :: String -> LexerState [Token]
lexInput [] = do
        lexOut <- LexState.getState
        pure . reverse $ lexOut
lexInput input@(c:cs)
        | isSpace c = lexInput cs
        | otherwise = do
                lexOut <- LexState.getState
                (tok, input') <- lexToken input
                LexState.putState (tok:lexOut)
                lexInput input'


lexToken :: String -> LexerState (Token, String)
lexToken [] = throwError ImpossibleError
lexToken input@(c:cs)
        | isSeparator c    = separator input
        | isTwoCharOp c cs = twoCharOperator input
        | isOpSymbol c     = operator input
        | identStart c     = identifier input
        | isDigit c        = number input
        | otherwise        = throwError (LexerError (BadInput [c]))


separator :: String -> LexerState (Token, String)
separator [] = throwError ImpossibleError
separator (c:cs) =
        case c of
             '(' -> pure (OpenParen, cs)
             ')' -> pure (CloseParen, cs)
             '{' -> pure (OpenBrace, cs)
             '}' -> pure (CloseBrace, cs)
             ';' -> pure (SemiColon, cs)
             ':' -> pure (Colon, cs)
             '?' -> pure (QuestMark, cs)
             ',' -> pure (Comma, cs)
             _   -> throwError $ LexerError (UnexpectedInput [c])


identifier :: String -> LexerState (Token, String)
identifier [] = throwError ImpossibleError
identifier (c:cs) =
        let (str, cs') = span isValidInIdentifier cs
            in
        case c:str of
             "int"      -> pure (Keyword Int, cs')
             "return"   -> pure (Keyword Return, cs')
             "if"       -> pure (Keyword If, cs')
             "else"     -> pure (Keyword Else, cs')
             "for"      -> pure (Keyword For, cs')
             "while"    -> pure (Keyword While, cs')
             "do"       -> pure (Keyword Do, cs')
             "break"    -> pure (Keyword Break, cs')
             "continue" -> pure (Keyword Continue, cs')
             _          -> pure (Ident (c:str), cs')


number :: String -> LexerState (Token, String)
number [] = throwError ImpossibleError
number (c:cs) = do
        let (digs, cs') = span isDigit cs
        pure (ConstInt $ read (c:digs), cs')


twoCharOperator :: String -> LexerState (Token, String)
twoCharOperator []  = throwError ImpossibleError
twoCharOperator [_] = throwError ImpossibleError
twoCharOperator (c:n:cs) =
        case c:[n] of
             "||" -> pure (OpTok PipePipe, cs)
             "&&" -> pure (OpTok AmpAmp, cs)
             ">=" -> pure (OpTok RightArrowEqual, cs)
             "<=" -> pure (OpTok LeftArrowEqual, cs)
             "==" -> pure (OpTok EqualEqual, cs)
             "!=" -> pure (OpTok BangEqual, cs)
             "+=" -> pure (OpTok PlusEqual, cs)
             "-=" -> pure (OpTok MinusEqual, cs)
             "*=" -> pure (OpTok AsteriskEqual, cs)
             "/=" -> pure (OpTok BackslashEqual, cs)
             "%=" -> pure (OpTok PercentEqual, cs)
             "++" -> pure (OpTok PlusPlus, cs)
             _    -> throwError $ LexerError (UnexpectedInput (c:[n]))


operator :: String -> LexerState (Token, String)
operator [] = throwError ImpossibleError
operator (c:cs) =
        case c of
             '+' -> pure (OpTok PlusSign, cs)
             '-' -> pure (OpTok MinusSign, cs)
             '*' -> pure (OpTok Asterisk, cs)
             '%' -> pure (OpTok Percent, cs)
             '/' -> pure (OpTok Backslash, cs)
             '~' -> pure (OpTok Tilde, cs)
             '!' -> pure (OpTok Bang, cs)
             '>' -> pure (OpTok RightArrow, cs)
             '<' -> pure (OpTok LeftArrow, cs)
             '=' -> pure (OpTok EqualSign, cs)
             '&' -> pure (OpTok Ampersand, cs)
             _   -> throwError $ LexerError (UnexpectedInput [c])


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
secondOpSymbols = "=|&+"


identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'
