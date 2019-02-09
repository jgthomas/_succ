
module SymTab (Evaluator(..),
               newSymTab,
               addVariable,
               checkVariable,
               labelNum,
               initScope,
               variableOffset,
               closeScope,
               stackPointerValue,
               setBreak,
               getBreak,
               setContinue,
               getContinue,
               initFunction,
               closeFunction) where


import Lexer
import Parser
import SimpleStack
import qualified Data.Map as M
import Control.Monad (liftM, ap)


data SymTab = Tab { scope     :: Int
                  , labelNo   :: Int
                  , offset    :: Int
                  , funcNames :: Stack String
                  , funcScope :: M.Map String Int
                  , funcVars  :: M.Map String (M.Map Int (M.Map String Int))
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


{-
- Exported API functions
-}

newSymTab :: SymTab
newSymTab = Tab
            (-1)
            0
            (-8)
            newStack
            M.empty
            M.empty
            M.empty


initFunction :: String -> Evaluator String
initFunction name = Ev $ \symTab ->
        let funcs = funcNames symTab
            scopes = funcScope symTab
            vars = funcVars symTab
            symTab' = symTab { funcNames = stackPush name funcs }
            symTab'' = symTab' { funcScope = M.insert name 0 scopes }
            symTab''' = symTab'' { funcVars = M.insert name M.empty vars }
            newVars = funcVars symTab'''
            in case M.lookup name newVars of
                    Just funcScope ->
                            let funcScope' = M.insert 0 M.empty funcScope
                                symTab'''' = symTab''' { funcVars = M.insert name funcScope' newVars }
                                in
                            (name, symTab'''')
                    Nothing -> error "Failed to initilise function"


closeFunction :: Evaluator Bool
closeFunction = Ev $ \symTab ->
        let funcs = funcNames symTab
            symTab' = symTab { funcNames = stackPop funcs }
            in
        (True, symTab')


initScope :: Evaluator Int
initScope = Ev $ \symTab ->
        let scopeTab = variables symTab
            initScope = scope symTab
            symTab' = symTab { scope = initScope + 1 }
            currScope = scope symTab'
            symTab'' = symTab' { variables = M.insert currScope M.empty scopeTab }
            in
        (currScope, symTab'')


closeScope :: Evaluator Int
closeScope = Ev $ \symTab ->
        let scopeTab = variables symTab
            currScope = scope symTab
            symTab' = symTab { scope = currScope - 1 }
            newScope = scope symTab'
            in
        (newScope, symTab')


stackPointerValue :: Evaluator Int
stackPointerValue = do
        currOff <- currentOffset
        return $ negate currOff


variableOffset :: String -> Evaluator Int
variableOffset varName = do
        currScope <- currentScope
        findOffset currScope varName


getBreak :: Evaluator Int
getBreak = do
        currScope <- currentScope
        findOffset currScope "@Break"


getContinue :: Evaluator Int
getContinue = do
        currScope <- currentScope
        findOffset currScope "@Continue"


setBreak :: Int -> Evaluator Int
setBreak labelNo = do
        currScope <- currentScope
        store currScope "@Break" labelNo


setContinue :: Int -> Evaluator Int
setContinue labelNo = do
        currScope <- currentScope
        store currScope "@Continue" labelNo


labelNum :: Evaluator Int
labelNum = Ev $ \symTab ->
        let num = labelNo symTab
            symTab' = symTab { labelNo = num + 1 }
            in
        (num, symTab')


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currScope <- currentScope
        checkVar currScope varName


addVariable :: String -> Evaluator Int
addVariable varName = do
        currScope <- currentScope
        currOff <- currentOffset
        store currScope varName currOff
        incrementOffset currOff


{-
- Internal functions
-}

findOffset :: Int -> String -> Evaluator Int
findOffset currScope varName =
        if currScope == noScope
           then error $ "Undefined variable: '" ++ varName ++ "'"
           else do
                   offset <- lookUp currScope varName
                   if offset == notFound
                      then findOffset (currScope-1) varName
                      else return offset


lookUp :: Int -> String -> Evaluator Int
lookUp currScope str = Ev $ \symTab ->
        let scopeTab = variables symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let value = M.lookup str scopeMap
                                in case value of
                                        Just v  -> (v, symTab)
                                        Nothing -> (notFound, symTab)
                    Nothing -> error "No scope currently defined"


store :: Int -> String -> Int -> Evaluator Int
store currScope name val = Ev $ \symTab ->
        let scopeTab = variables symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let scopeMap' = M.insert name val scopeMap
                                symTab'  = symTab { variables = M.insert currScope scopeMap' scopeTab }
                                in
                            (val, symTab')
                    Nothing -> error "No scope currently defined"


checkVar :: Int -> String -> Evaluator Bool
checkVar currScope varName = Ev $ \symTab ->
        let scopeTab = variables symTab
            in case M.lookup currScope scopeTab of
                    Just scopeMap ->
                            let value = M.lookup varName scopeMap
                                in case value of
                                        Just v  -> (True, symTab)
                                        Nothing -> (False, symTab)
                    Nothing -> error "No scope currently defined"


currentFunction :: Evaluator String
currentFunction = Ev $ \symTab ->
        let nameStack = funcNames symTab
            currFunc  = stackPeek nameStack
            in
        (currFunc, symTab)


currentScope :: Evaluator Int
currentScope = Ev $ \symTab ->
        let currScope = scope symTab
            in
        (currScope, symTab)


currentOffset :: Evaluator Int
currentOffset = Ev $ \symTab ->
        let currOff = offset symTab
            in
        (currOff, symTab)


incrementOffset :: Int -> Evaluator Int
incrementOffset currOff = Ev $ \symTab ->
        let symTab' = symTab { offset = currOff + (-8) }
            in
        (currOff, symTab')


notFound :: Int
notFound = -1


noScope :: Int
noScope = (-1)
