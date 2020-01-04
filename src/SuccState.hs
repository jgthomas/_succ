{-|
Module       : SuccState
Description  : Core compiler state definition

Defines the core monad transformer stack used in the compiler, combining
a State monad with an error type.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuccState
        (SuccStateM,
         getState,
         putState,
         throwError,
         runSuccState
        ) where


import Control.Monad.State        (State, evalState, get, lift, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Error                      (CompilerError)


-- | Wrapper type for the compiler state
newtype SuccStateM s a = CM {
        unCM :: ExceptT CompilerError (State s) a
} deriving (Functor, Applicative, Monad)


-- | Extract the contents of the state
getState :: SuccStateM a a
getState = CM (lift get)


-- | Update the contents of the state
putState :: s -> SuccStateM s ()
putState s = CM $ lift $ put s


-- | Throw an error
throwError :: CompilerError -> SuccStateM s a
throwError e = CM $ throwE e


-- | Run the state extracting the error or result
runSuccState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runSuccState f t s = evalState (runExceptT . unCM $ f t) s
