
module NewLexer (tokenize) where


import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Char (isDigit, isAlpha, isSpace)

import Tokens (Operator(..), Keyword(..), Token(..))
import Error  (CompilerError(LexerError), LexerError(..))


type LexerOutput = [Token]
type LexerState = State LexerOutput
type LexerM m = ExceptT CompilerError m


startState :: [Token]
startState = []


tokenize :: [Char] -> Either CompilerError [Token]
tokenize input = evalState (runExceptT $ (lexInput input)) startState


lexInput :: [Char] -> LexerM LexerState LexerOutput
lexInput [] = do
        lexOut <- get
        return lexOut
lexInput input@(c:cs) = do
        lexOut <- get
        let (tok, input') = getToken input
        case tok of
             TokUnrecognised -> throwE (LexerError (BadToken [c]))
             TokSpace        -> put lexOut
             _               -> put (lexOut ++ [tok])
        lexInput input'


getToken :: [Char] -> (Token, [Char])
getToken [] = (TokUnrecognised, [])
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
