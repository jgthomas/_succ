
module Error (CompilerError(..),
              LexerError(..),
              CompilerM) where


import Control.Monad.Trans.Except


data CompilerError = LexerError LexerError
                   | ImpossibleError
                   deriving Show


data LexerError = BadToken String
                | EmptyInput
                deriving Show


type CompilerM m = ExceptT CompilerError m
