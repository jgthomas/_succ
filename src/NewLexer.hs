
module NewLexer (tokenize) where


import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isDigit, isAlpha, isSpace)

import Tokens (Operator(..), Keyword(..), Token(..))
import Error  (CompilerError(LexerError), LexerError(..), CompilerM)


type LexerState = State [Token]


tokenize :: [Char] -> Either CompilerError [Token]
tokenize input = evalState (runLexer input) []


runLexer :: [Char] -> LexerState (Either CompilerError [Token])
runLexer input = runExceptT $ lexer input


lexer :: [Char] -> CompilerM LexerState [Token]
lexer []    = throwE (LexerError EmptyInput)
lexer input = lexInput input


lexInput :: [Char] -> CompilerM LexerState [Token]
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


getToken :: [Char] -> (Token, [Char])
getToken [] = (TokNull, [])
getToken (c:cs)
    | c == '('          = (TokOpenParen, cs)
    | c == ')'          = (TokCloseParen, cs)
    | c == '{'          = (TokOpenBrace, cs)
    | c == '}'          = (TokCloseBrace, cs)
    | c == ';'          = (TokSemiColon, cs)
    | identifierStart c = identifier c cs
    | isDigit c         = number c cs
    | isSpace c         = (TokSpace, cs)
    | otherwise         = (TokUnrecognised, cs)


identifier :: Char -> [Char] -> (Token, [Char])
identifier c cs =
    let (str, cs') = span isValidInIdentifier cs
        in
    case c:str of
         "int"      -> (TokKeyword Int, cs')
         "return"   -> (TokKeyword Return, cs')
         _          -> (TokIdent (c:str), cs')


number :: Char -> [Char] -> (Token, [Char])
number c cs =
    let (digs, cs') = span isDigit cs
        in
    (TokConstInt (read (c:digs)), cs')


identifierStart :: Char -> Bool
identifierStart c = isAlpha c || c == '_'


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identifierStart c || isDigit c
