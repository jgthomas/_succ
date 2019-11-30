
module Error
        (CompilerError(..),
         LexerError(..),
         ParserError(..)
        ) where


data CompilerError = LexerError LexerError
                   | ParserError ParserError
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = BadInput String
                | EmptyInput
                deriving (Show, Eq)


data ParserError = ParseError String
                 | MissingToken String
                 | NoTokens
                 deriving (Show, Eq)
