{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuccState
        (CompilerM,
         getState,
         putState,
         throwError,
         runSuccState
        ) where


import Control.Monad.State
import Control.Monad.Trans.Except

import Error (CompilerError)


newtype CompilerM s a = CM {
        unCM :: ExceptT CompilerError (State s) a
} deriving (Functor, Applicative, Monad)


getState :: CompilerM a a
getState = CM (lift get)


putState :: s -> CompilerM s ()
putState s = CM $ lift $ put s


throwError :: CompilerError -> CompilerM s a
throwError e = CM $ throwE e


runSuccState :: (t -> CompilerM s a) -> t -> s -> Either CompilerError a
runSuccState f t s = evalState (runExceptT . unCM $ f t) s
