
module Evaluator (Evaluator(Ev)) where


import Control.Monad (liftM, ap)

import Types (SymTab)


newtype Evaluator a = Ev (SymTab -> (a, SymTab))


{-
-  Monad is now defined as a subclass of applicative
-  and so new monad definitions require that they also
-  be specified as applicatives (and functors, of which
-  applicatives are a subclass)
-
-  See: https://stackoverflow.com/a/34641501/5671759
-}
instance Functor Evaluator where
        -- <$> infix
        fmap = liftM


instance Applicative Evaluator where
        pure  = return
        (<*>) = ap


instance Monad Evaluator where
        -- return :: a -> ma
        return a = Ev (\symTab -> (a, symTab))

        -- (>>=)  :: ma -> (a -> mb) -> mb
        (Ev act) >>= k = Ev $ \symTab ->
                let (a, symTab') = act symTab
                    (Ev act')    = k a
                    in
                act' symTab'
