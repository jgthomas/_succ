
module SymTab (Evaluator(..), SymTab(..), addSymbol, lookUp, checkVar) where


import Lexer
import Parser
import qualified Data.Map as M
import Control.Monad (liftM, ap)


data SymTab = Tab { labelNo   :: Int
                  , offset    :: Int
                  , variables :: M.Map String Int}
            deriving Show


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
        fmap = liftM


instance Applicative Evaluator where
        pure  = return
        (<*>) = ap


instance Monad Evaluator where
        (Ev act) >>= k = Ev $
            \symTab ->
                    let (x, symTab') = act symTab
                        (Ev act')    = k x
                    in act' symTab'
        return x = Ev (\symTab -> (x, symTab))


checkVar :: String -> Evaluator Bool
checkVar str = Ev $ \symTab ->
        let tab = variables symTab
            in case M.lookup str tab of
                    Just v  -> (True, symTab)
                    Nothing -> (False, symTab)


lookUp :: String -> Evaluator Int
lookUp str = Ev $ \symTab ->
        let tab = variables symTab
            in case M.lookup str tab of
                    Just v  -> (v, symTab)
                    Nothing -> error $ "Undefined variable: '" ++ str ++ "'"


addSymbol :: String -> Evaluator Int
addSymbol str = Ev $ \symTab ->
        let tab = variables symTab
            off = offset symTab
            symTab'  = symTab { variables = M.insert str off tab }
            symTab'' = symTab' { offset = off + (-8) }
            in
        (off, symTab'')
