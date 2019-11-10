
module Error (CompilerError(..),
              LexerError(..)) where



data CompilerError = LexerError LexerError
                   deriving Show


data LexerError = BadToken String
                | EmptyInput
                deriving Show
