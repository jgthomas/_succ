{-|
Module       : Lexer
Description  : Tokenizes an input string

Processes a string representing a program written in C into
a list of tokens.
-}
module Lexer.Lexer (tokenize) where


import           Data.Char      (isAlpha, isDigit, isSpace)

import           Lexer.LexState (LexerState, throwError)
import qualified Lexer.LexState as LexState (addToken, evaluate, getState,
                                             incLineNum, mkLexDat, startState)
import           Types.Error    (CompilerError (ImpossibleError, LexerError),
                                 LexerError (..))
import           Types.Tokens


-- | Convert a string representing a C program to a list of tokens
tokenize :: String -> Either CompilerError [Token]
tokenize inputCode = LexState.evaluate lexer inputCode LexState.startState


lexer :: String -> LexerState [Token]
lexer []        = throwError (LexerError EmptyInput)
lexer inputCode = lexInput inputCode


lexInput :: String -> LexerState [Token]
lexInput [] = do
        lexOut <- LexState.getState
        pure . reverse $ lexOut
lexInput inputCode@(c:cs)
        | c == '\n' = do
                LexState.incLineNum
                lexInput cs
        | isSpace c = lexInput cs
        | otherwise = do
                (tok, inputCode') <- lexToken inputCode
                LexState.addToken tok
                lexInput inputCode'


lexToken :: String -> LexerState (Token, String)
lexToken [] = throwError ImpossibleError
lexToken inputCode@(c:_)
        | isSeparator c = lexSeparator inputCode
        | isOpSymbol c  = lexOperator inputCode
        | identStart c  = lexIdentifier inputCode
        | isDigit c     = lexNumber inputCode
        | otherwise     = throwError $ LexerError (UnexpectedInput inputCode)


lexSeparator :: String -> LexerState (Token, String)
lexSeparator [] = throwError ImpossibleError
lexSeparator (c:cs) = do
        dat <- LexState.mkLexDat [c]
        case c of
             '(' -> pure (OpenBracket OpenParen dat, cs)
             '{' -> pure (OpenBracket OpenBrace dat, cs)
             '[' -> pure (OpenBracket OpenSqBracket dat, cs)
             ')' -> pure (CloseBracket CloseParen dat, cs)
             '}' -> pure (CloseBracket CloseBrace dat, cs)
             ']' -> pure (CloseBracket CloseSqBracket dat, cs)
             ';' -> pure (Separator SemiColon dat, cs)
             ':' -> pure (Separator Colon dat, cs)
             '?' -> pure (Separator QuestMark dat, cs)
             ',' -> pure (Separator Comma dat, cs)
             _   -> throwError $ LexerError (UnexpectedInput [c])


lexIdentifier :: String -> LexerState (Token, String)
lexIdentifier [] = throwError ImpossibleError
lexIdentifier (c:cs) = do
        let (str, cs') = span isValidInIdentifier cs
        dat <- LexState.mkLexDat (c:str)
        case c:str of
             "int"      -> pure (Keyword Int dat, cs')
             "return"   -> pure (Keyword Return dat, cs')
             "if"       -> pure (Keyword If dat, cs')
             "else"     -> pure (Keyword Else dat, cs')
             "for"      -> pure (Keyword For dat, cs')
             "while"    -> pure (Keyword While dat, cs')
             "do"       -> pure (Keyword Do dat, cs')
             "break"    -> pure (Keyword Break dat, cs')
             "continue" -> pure (Keyword Continue dat, cs')
             _          -> pure (Ident (c:str) dat, cs')


lexNumber :: String -> LexerState (Token, String)
lexNumber [] = throwError ImpossibleError
lexNumber (c:cs) = do
        let (digs, cs') = span isDigit cs
            digits      = c:digs
        dat <- LexState.mkLexDat digits
        pure (ConstInt (read digits) dat, cs')


lexOperator :: String -> LexerState (Token, String)
lexOperator []    = throwError ImpossibleError
lexOperator [a]   = oneCharOperator [a]
lexOperator [a,b] = twoCharOperator [a,b]
lexOperator inputCode@(c:n:m:cs) = do
        dat <- LexState.mkLexDat (c:n:[m])
        case c:n:[m] of
             "<<=" -> pure (OpTok DoubleLArrowEqual dat, cs)
             ">>=" -> pure (OpTok DoubleRArrowEqual dat, cs)
             _     -> twoCharOperator inputCode


twoCharOperator :: String -> LexerState (Token, String)
twoCharOperator []  = throwError ImpossibleError
twoCharOperator [a] = oneCharOperator [a]
twoCharOperator inputCode@(c:n:cs) = do
        dat <- LexState.mkLexDat (c:[n])
        case c:[n] of
             "||" -> pure (OpTok PipePipe dat, cs)
             "&&" -> pure (OpTok AmpAmp dat, cs)
             ">=" -> pure (OpTok RightArrowEqual dat, cs)
             "<=" -> pure (OpTok LeftArrowEqual dat, cs)
             "==" -> pure (OpTok EqualEqual dat, cs)
             "!=" -> pure (OpTok BangEqual dat, cs)
             "+=" -> pure (OpTok PlusEqual dat, cs)
             "-=" -> pure (OpTok MinusEqual dat, cs)
             "*=" -> pure (OpTok AsteriskEqual dat, cs)
             "/=" -> pure (OpTok BackslashEqual dat, cs)
             "%=" -> pure (OpTok PercentEqual dat, cs)
             "++" -> pure (OpTok PlusPlus dat, cs)
             "--" -> pure (OpTok MinusMinus dat, cs)
             "&=" -> pure (OpTok AmpEqual dat, cs)
             "^=" -> pure (OpTok CaretEqual dat, cs)
             "|=" -> pure (OpTok PipeEqual dat, cs)
             "<<" -> pure (OpTok DoubleLeftArrow dat, cs)
             ">>" -> pure (OpTok DoubleRightArrow dat, cs)
             _    -> oneCharOperator inputCode


oneCharOperator :: String -> LexerState (Token, String)
oneCharOperator [] = throwError ImpossibleError
oneCharOperator inputCode@(c:cs) = do
        dat <- LexState.mkLexDat [c]
        case c of
             '+' -> pure (OpTok PlusSign dat, cs)
             '-' -> pure (OpTok MinusSign dat, cs)
             '*' -> pure (OpTok Asterisk dat, cs)
             '%' -> pure (OpTok Percent dat, cs)
             '/' -> pure (OpTok Backslash dat, cs)
             '~' -> pure (OpTok Tilde dat, cs)
             '!' -> pure (OpTok Bang dat, cs)
             '>' -> pure (OpTok RightArrow dat, cs)
             '<' -> pure (OpTok LeftArrow dat, cs)
             '=' -> pure (OpTok EqualSign dat, cs)
             '&' -> pure (OpTok Ampersand dat, cs)
             '^' -> pure (OpTok Caret dat, cs)
             '|' -> pure (OpTok Pipe dat, cs)
             _   -> throwError $ LexerError (UnexpectedInput inputCode)


isSeparator :: Char -> Bool
isSeparator c = c `elem` separators


isOpSymbol :: Char -> Bool
isOpSymbol c = c `elem` opSymbols


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identStart c || isDigit c


opSymbols :: String
opSymbols = "+-*/~!|&<>=%^"


separators :: String
separators = "(){};:,?[]"


identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'
