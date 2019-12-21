
module Tokens where


data Operator = PlusSign
              | MinusSign
              | Asterisk
              | BackSlash
              | Percent
              | Tilde
              | Bang
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
              deriving (Show, Eq)


unary :: [Operator]
unary = [MinusSign,Tilde,Bang]


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
           | Colon
           | QuestMark
           | Comma
           | Ampersand
           | Wut
           deriving (Show, Eq)
