
module Error where


import Tokens (Keyword, Token, Operator)
import AST    (Tree)


data CompilerError = LexerError LexerError
                   | ParserError ParserError
                   | SyntaxError SyntaxError
                   | TypeError TypeError
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = BadInput String
                | EmptyInput
                deriving (Show, Eq)


data ParserError = ParseError String
                 | TreeError Tree
                 | TokensError [Token]
                 deriving (Show, Eq)


data SyntaxError = InvalidIdentifier Token
                 | MissingToken Token
                 | UnexpectedToken Token
                 | MissingIdentifier
                 | MissingKeyword Keyword
                 | Undeclared Tree
                 | Undefined Tree
                 | DoubleDeclared Tree
                 | DoubleDefined Tree
                 | Unrecognised Tree
                 | Unexpected Tree
                 | MisMatch Int Tree
                 | InvalidCall Tree
                 | UnexpectedOp Operator
                 deriving (Show, Eq)


data TypeError = InvalidType Token
               | TypeMismatch Token Token
               deriving (Show, Eq)

