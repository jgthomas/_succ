
module Lexer (Token(..),
              Keyword(..),
              UnaryOperator(..),
              tokenize, lookAhead, accept) where


import Data.Char


data UnaryOperator = Negation
                   | BitwiseCompl
                   | LogicNegation
                   deriving (Show, Eq)


data Keyword = Int
             | Return
             deriving (Show, Eq)


data Token = TokOpenParen
           | TokCloseParen
           | TokOpenBrace
           | TokCloseBrace
           | TokSemiColon
           | TokUnary UnaryOperator
           | TokIdent String
           | TokConstInt Int
           | TokKeyword Keyword
           | TokEnd
           deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | c == '('      = TokOpenParen                  : tokenize cs
    | c == ')'      = TokCloseParen                 : tokenize cs
    | c == '{'      = TokOpenBrace                  : tokenize cs
    | c == '}'      = TokCloseBrace                 : tokenize cs
    | c == ';'      = TokSemiColon                  : tokenize cs
    | elem c "-~!"  = TokUnary (unaryOperator c)    : tokenize cs
    | isAlpha c     = identifier c cs
    | isDigit c     = number c cs
    | isSpace c     = tokenize cs
    | otherwise     = error $ "Cannot tokenize " ++ [c]


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs
                      in case (c:str) of
                              "int"    -> TokKeyword Int    : tokenize cs'
                              "return" -> TokKeyword Return : tokenize cs'
                              _        -> TokIdent (c:str)  : tokenize cs'


number :: Char -> String -> [Token]
number c cs =
    let (digs, cs') = span isDigit cs in
    TokConstInt (read (c:digs)) : tokenize cs'


unaryOperator :: Char -> UnaryOperator
unaryOperator c | c == '-' = Negation
                | c == '~' = BitwiseCompl
                | c == '!' = LogicNegation
