
module Tokens (Operator(..),
               Keyword(..),
               Token(..)) where


data Operator = Plus
              | Minus
              | Multiply
              | Divide
              | Modulo
              | BitwiseCompl
              | LogicNegation
              | LogicalOR
              | LogicalAND
              | GreaterThan
              | GreaterThanOrEqual
              | LessThan
              | LessThanOrEqual
              | Equal
              | NotEqual
              | Assign
              deriving (Show, Eq)


data Keyword = Int
             | Return
             | If
             | Else
             | For
             | While
             | Do
             | Break
             | Continue
             deriving (Show, Eq)


data Token = TokOpenParen
           | TokCloseParen
           | TokOpenBrace
           | TokCloseBrace
           | TokSemiColon
           | TokOp Operator
           | TokIdent String
           | TokConstInt Int
           | TokKeyword Keyword
           | TokEnd
           | TokColon
           | TokQuestMark
           | TokComma
           deriving (Show, Eq)
