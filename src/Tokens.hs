
module Tokens where


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
              | PlusAssign
              | MinusAssign
              | MultiplyAssign
              | DivideAssign
              | ModuloAssign
              | Ampersand
              deriving (Show, Eq)


unary :: [Operator]
unary = [Minus,BitwiseCompl,LogicNegation]


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


data Token = OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | SemiColon
           | Op Operator
           | Ident String
           | ConstInt Int
           | Keyword Keyword
           | End
           | Colon
           | QuestMark
           | Comma
           | Space
           | Wut
           deriving (Show, Eq)
