
module SymTab (Evaluator(..), SymTab(..), addSymbol) where


import Lexer
import Parser
import qualified Data.Map as M
import Control.Monad (liftM, ap)


--type SymTab = M.Map String Int
--type SymTab = ()


data SymTab = Tab { offset    :: Int
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



--lookUp :: String -> Evaluator Int
--lookUp str = Ev $ \symTab ->
--        case M.lookup str symTab of
--             Just v  -> (v, symTab)
--             Nothing -> error $ "Undefined variable: " ++ str
--
--
--addSymbol :: String -> Int -> Evaluator Int
--addSymbol str val = Ev $ \symTab ->
--        let symTab' = M.insert str val symTab
--            in (val, symTab')


addSymbol :: String -> Evaluator Int
addSymbol str = Ev $ \symTab ->
        let tab = variables symTab
            off = offset symTab
            symTab' = symTab { variables = M.insert str off tab }
            symTab'' = symTab' { offset = off + (-8) }
            in
        (off, symTab'')
