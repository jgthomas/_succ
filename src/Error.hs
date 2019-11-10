
module Error (CompilerError(..),
              LexerError(..),
              CompilerM) where


import Control.Monad.Trans.Except


data CompilerError = LexerError LexerError
                   deriving Show


data LexerError = BadToken String
                | EmptyInput
                deriving Show


type CompilerM m = ExceptT CompilerError m
