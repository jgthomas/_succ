--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuccState (CompilerM) where


import Control.Monad.Trans.Except (ExceptT)

import Error (CompilerError)


type CompilerM m = ExceptT CompilerError m


--newtype CompilerM m a = CompilerM {
--        unCompilerM :: ExceptT CompilerError m a
--} deriving (Show, Functor, Applicative, Monad)
