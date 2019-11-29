
module Error (CompilerError(..),
              LexerError(..)) where


data CompilerError = LexerError LexerError
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = BadInput String
                | EmptyInput
                deriving (Show, Eq)
