
module Error (CompilerError(..),
              LexerError(..)) where


data CompilerError = LexerError LexerError
                   | ImpossibleError
                   deriving Show


data LexerError = BadToken String
                | EmptyInput
                deriving Show
