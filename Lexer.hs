
module Lexer (tokenize) where


import Data.Char

import Tokens (Operator(..), Keyword(..), Token(..))


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | c == '('          = TokOpenParen              : tokenize cs
    | c == ')'          = TokCloseParen             : tokenize cs
    | c == '{'          = TokOpenBrace              : tokenize cs
    | c == '}'          = TokCloseBrace             : tokenize cs
    | c == ';'          = TokSemiColon              : tokenize cs
    | c == ':'          = TokColon                  : tokenize cs
    | c == '?'          = TokQuestMark              : tokenize cs
    | c == ','          = TokComma                  : tokenize cs
    | isTwoCharOp c cs  = twoCharOperator c cs
    | c == '='          = TokAssign                 : tokenize cs
    | elem c opSymbols  = TokOp (operator c)        : tokenize cs
    | identifierStart c = identifier c cs
    | isDigit c         = number c cs
    | isSpace c         = tokenize cs
    | otherwise         = error $ "Cannot tokenize " ++ [c]


identifier :: Char -> String -> [Token]
identifier c cs =
    let (str, cs') = span isValidInIdentifier cs
        in
    case (c:str) of
         "int"      -> TokKeyword Int       : tokenize cs'
         "return"   -> TokKeyword Return    : tokenize cs'
         "if"       -> TokKeyword If        : tokenize cs'
         "else"     -> TokKeyword Else      : tokenize cs'
         "for"      -> TokKeyword For       : tokenize cs'
         "while"    -> TokKeyword While     : tokenize cs'
         "do"       -> TokKeyword Do        : tokenize cs'
         "break"    -> TokKeyword Break     : tokenize cs'
         "continue" -> TokKeyword Continue  : tokenize cs'
         _          -> TokIdent (c:str)     : tokenize cs'


number :: Char -> String -> [Token]
number c cs =
    let (digs, cs') = span isDigit cs in
    TokConstInt (read (c:digs)) : tokenize cs'


twoCharOperator :: Char -> String -> [Token]
twoCharOperator c cs =
        let (so, cs') = span (\x -> elem x secondOpSymbols) cs
            in
        case (c:so) of
             "||" -> TokOp LogicalOR          : tokenize cs'
             "&&" -> TokOp LogicalAND         : tokenize cs'
             ">=" -> TokOp GreaterThanOrEqual : tokenize cs'
             "<=" -> TokOp LessThanOrEqual    : tokenize cs'
             "==" -> TokOp Equal              : tokenize cs'
             "!=" -> TokOp NotEqual           : tokenize cs'
             _    -> error "Unrecognised two character operator"


isTwoCharOp :: Char -> String -> Bool
isTwoCharOp c [] = False
isTwoCharOp c cs = elem c opSymbols
                   && elem (head cs) secondOpSymbols


operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Multiply
           | c == '%' = Modulo
           | c == '/' = Divide
           | c == '~' = BitwiseCompl
           | c == '!' = LogicNegation
           | c == '>' = GreaterThan
           | c == '<' = LessThan


opSymbols :: String
opSymbols = "+-*/~!|&<>=%"


secondOpSymbols :: String
secondOpSymbols = "=|&"


identifierStart :: Char -> Bool
identifierStart c = isAlpha c || c == '_'


isValidInIdentifier :: Char -> Bool
isValidInIdentifier c = identifierStart c || isDigit c
