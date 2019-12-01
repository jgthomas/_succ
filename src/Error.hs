
module Error
        (
         CompilerError(..),
         LexerError(..),
         ParserError(..),
         SyntaxError(..),
         TypeError(..)
        ) where


import Tokens (Operator(..),
               Keyword(..),
               Token(..)
              )

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
                 | NoTokens
                 deriving (Show, Eq)


data SyntaxError = InvalidIdentifier Token
                 | MissingToken Token
                 deriving (Show, Eq)


data TypeError = InvalidType Token
               | TypeMismatch Token Token
               deriving (Show, Eq)

