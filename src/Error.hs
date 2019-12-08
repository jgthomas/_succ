
module Error
        (
         CompilerError(..),
         LexerError(..),
         ParserError(..),
         SyntaxError(..),
         TypeError(..)
        ) where


import Tokens (Keyword,
               Token)
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
                 deriving (Show, Eq)


data TypeError = InvalidType Token
               | TypeMismatch Token Token
               deriving (Show, Eq)

