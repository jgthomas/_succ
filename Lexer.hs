module Lexer (Token(..), tokenize, lookAhead, accept) where


import Data.Char


data Token = TokOpenParen
           | TokCloseParen
           | TokOpenBrace
           | TokCloseBrace
           | TokSemiColon
           | TokIntConst Int
           | TokEnd
           deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | c == '('      = TokOpenParen       : tokenize cs
    | c == ')'      = TokCloseParen      : tokenize cs
    | c == '{'      = TokOpenBrace       : tokenize cs
    | c == '}'      = TokCloseBrace      : tokenize cs
    | c == ';'      = TokSemiColon       : tokenize cs
    | isDigit c     = number c cs
    | isSpace c     = tokenize cs
    | otherwise     = error $ "Cannot tokenize " ++ [c]


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


--identifier :: Char -> String -> [Token]
--identifier c cs = let (str, cs') = span isAlphaNum cs in
--                  TokIdent (c:str) : tokenize cs'
--
--
number :: Char -> String -> [Token]
number c cs =
    let (digs, cs') = span isDigit cs in
    TokIntConst (read (c:digs)) : tokenize cs'
