{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuccState
        (LexerState,
         ParserState,
         GenState,
         SuccStateM,
         getState,
         putState,
         throwError,
         runSuccState
        ) where


import Control.Monad.State
import Control.Monad.Trans.Except

import Error  (CompilerError)
import Tokens (Token)
import AST    (Tree)
import Types  (SymTab)


newtype SuccStateM s a = CM {
        unCM :: ExceptT CompilerError (State s) a
} deriving (Functor, Applicative, Monad)


type LexerState = SuccStateM [Token]
type ParserState = SuccStateM Tree
type GenState = SuccStateM SymTab


getState :: SuccStateM a a
getState = CM (lift get)


putState :: s -> SuccStateM s ()
putState s = CM $ lift $ put s


throwError :: CompilerError -> SuccStateM s a
throwError e = CM $ throwE e


runSuccState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runSuccState f t s = evalState (runExceptT . unCM $ f t) s
