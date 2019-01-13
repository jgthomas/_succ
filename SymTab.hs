
module SymTab (Evaluator(..)) where


import Lexer
import Parser
import qualified Data.Map as M
import Control.Monad (liftM, ap)


--type SymTab = M.Map String Int
type SymTab = ()


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



--lookUp :: String -> Evaluator String
--lookUp str = Ev $ \symTab ->
--        case M.lookup str symTab of
--             Just v  -> (v, symTab)
--             Nothing -> error $ "Undefined variable: " ++ str
--
--
--addSymbol :: String -> Int -> Evaluator String
--addSymbol str val = Ev $ \symTab ->
--        let symTab' = M.insert str val symTab
--            in (val, symTab')
