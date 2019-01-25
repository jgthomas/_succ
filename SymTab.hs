
module SymTab (Evaluator(..),
               SymTab(..),
               addSymbol,
               lookUp, checkVar, labelNum, initScope) where


import Lexer
import Parser
import qualified Data.Map as M
import Control.Monad (liftM, ap)


data SymTab = Tab { scope     :: Int
                  , labelNo   :: Int
                  , offset    :: Int
                  , variables :: M.Map Int (M.Map String Int)}
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
        let scopeTab = variables symTab
            currScope = scope symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let value = M.lookup str scopeMap
                                in case value of
                                        Just v  -> (True, symTab)
                                        Nothing -> (False, symTab)
                    Nothing -> error "No scope currently defined"


lookUp :: String -> Evaluator Int
lookUp str = Ev $ \symTab ->
        let scopeTab = variables symTab
            currScope = scope symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let value = M.lookup str scopeMap
                                in case value of
                                        Just v  -> (v, symTab)
                                        Nothing -> error $ "Undefined variable: '" ++ str ++ "'"
                    Nothing -> error "No scope currently defined"


addSymbol :: String -> Evaluator Int
addSymbol str = Ev $ \symTab ->
        let scopeTab = variables symTab
            off = offset symTab
            currScope = scope symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let scopeMap' = M.insert str off scopeMap
                                symTab'  = symTab { variables = M.insert currScope scopeMap' scopeTab }
                                symTab'' = symTab' { offset = off + (-8) }
                                in
                            (off, symTab'')
                    Nothing -> error "No scope currently defined"


labelNum :: Evaluator Int
labelNum = Ev $ \symTab ->
        let num = labelNo symTab
            symTab' = symTab { labelNo = num + 1 }
            in
        (num, symTab')


initScope :: Evaluator Int
initScope = Ev $ \symTab ->
        let scopeTab = variables symTab
            currScope = scope symTab
            symTab' = symTab { variables = M.insert currScope M.empty scopeTab }
            in
        (currScope, symTab')
