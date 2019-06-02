
module Scope (initScope,
              closeScope,
              initFunction,
              closeFunction,
              functionDefined,
              getBreak,
              setBreak,
              getContinue,
              setContinue,
              checkVariable,
              variableOffset,
              storeVar) where


import qualified Data.Map as M

import SimpleStack (currentFunction, popFunctionName, pushFunctionName)
import FunctionState (newFuncState)
import Evaluator (Evaluator(Ev))
import Types (SymTab(scopeLevels, scopesData),
              LocalScope,
              FunctionScope,
              ProgramScope)


{- API -}

initFunction :: String -> Evaluator ()
initFunction name = do
        pushFunctionName name
        newScopeRecord name
        newFuncScopesData name
        newFuncState name
        return ()


closeFunction :: Evaluator Bool
closeFunction = do
        popFunctionName


initScope :: Evaluator ()
initScope = do
        currFuncName <- currentFunction
        newScopeLevel <- incrementScope
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        funcScope' <- updateFunctionScope newScopeLevel M.empty funcScope
        updateProgramScope currFuncName funcScope'
        return ()


closeScope :: Evaluator Int
closeScope = do
        decrementScope


functionDefined :: String -> Evaluator Bool
functionDefined funcName = do
        progScope <- getProgramScope
        case M.lookup funcName progScope of
             Just fScope -> return True
             Nothing     -> return False


getBreak :: Evaluator (Maybe Int)
getBreak = do
        getOffset "@Break"


getContinue :: Evaluator (Maybe Int)
getContinue = do
        getOffset "@Continue"


setBreak :: Int -> Evaluator ()
setBreak labelNo = do
        store "@Break" labelNo


setContinue :: Int -> Evaluator ()
setContinue labelNo = do
        store "@Continue" labelNo


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        locScope <- getLocalScope scopeLevel funcScope
        case getVar varName locScope of
             Just v  -> return True
             Nothing -> return False


variableOffset :: String -> Evaluator (Maybe Int)
variableOffset name = do
        getOffset name


newScopeRecord :: String -> Evaluator Int
newScopeRecord name = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            symTab' = symTab { scopeLevels = M.insert name baseScope scopes }
            in
        (baseScope, symTab')


newFuncScopesData :: String -> Evaluator ()
newFuncScopesData name = do
        progScope <- updateProgramScope name M.empty
        funcScope <- getFunctionScope name progScope
        funcScope' <- updateFunctionScope baseScope M.empty funcScope
        updateProgramScope name funcScope'
        return ()


storeVar :: String -> Int -> Evaluator ()
storeVar varName off = do
        store varName off


{- Internal -}

getOffset :: String -> Evaluator (Maybe Int)
getOffset name = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        findOffset currFuncName scopeLevel name


findOffset :: String -> Int -> String -> Evaluator (Maybe Int)
findOffset func scope name =
        if scope == scopeLimit
           then return Nothing
           else do
                   offset <- lookUp func scope name
                   case offset of
                        Nothing  -> findOffset func (pred scope) name
                        Just off -> return (Just off)


lookUp :: String -> Int -> String -> Evaluator (Maybe Int)
lookUp func scope name = do
        progScope <- getProgramScope
        funcScope <- getFunctionScope func progScope
        locScope <- getLocalScope scope funcScope
        return $ getVar name locScope


store :: String -> Int -> Evaluator ()
store name value = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        locScope <- getLocalScope scopeLevel funcScope
        locScope' <- storeVariable name value locScope
        funcScope' <- updateFunctionScope scopeLevel locScope' funcScope
        updateProgramScope currFuncName funcScope'
        return ()


getLocalScope :: Int -> FunctionScope -> Evaluator LocalScope
getLocalScope scopeLevel funcScope = Ev $ \symTab ->
        case M.lookup scopeLevel funcScope of
             Just locScope -> (locScope, symTab)
             Nothing       -> error "No scope defined for function"


getFunctionScope :: String -> ProgramScope -> Evaluator FunctionScope
getFunctionScope funcName progScope = Ev $ \symTab ->
        case M.lookup funcName progScope of
             Just funcScope -> (funcScope, symTab)
             Nothing        -> error "No function scopes defined"


getProgramScope :: Evaluator ProgramScope
getProgramScope = Ev $ \symTab ->
        let scopes = scopesData symTab
            in
        (scopes, symTab)


storeVariable :: String -> Int -> LocalScope -> Evaluator LocalScope
storeVariable varName value locScope =
        let locScope' = M.insert varName value locScope
            in
        return locScope'


updateFunctionScope :: Int -> LocalScope -> FunctionScope -> Evaluator FunctionScope
updateFunctionScope scopeLevel locScope funcScope =
        let funcScope' = M.insert scopeLevel locScope funcScope
            in
        return funcScope'


updateProgramScope :: String -> FunctionScope -> Evaluator ProgramScope
updateProgramScope funcName funcScope = Ev $ \symTab ->
        let scopes = scopesData symTab
            symTab' = symTab { scopesData = M.insert funcName funcScope scopes }
            scopes' = scopesData symTab'
            in
        (scopes', symTab')


getVar :: String -> LocalScope -> Maybe Int
getVar varName varMap =
        case M.lookup varName varMap of
             Just v  -> Just v
             Nothing -> Nothing


incrementScope :: Evaluator Int
incrementScope = do
        stepScope succ


decrementScope :: Evaluator Int
decrementScope = do
        stepScope pred


findScope :: String -> Evaluator Int
findScope name = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            scopeLevel = M.lookup name scopes
            in
        case scopeLevel of
             Just scope -> (scope, symTab)
             Nothing    -> error $ "No scopes defined for function " ++ name


stepScope :: (Int -> Int) -> Evaluator Int
stepScope func = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        switchScope currFuncName $ func scopeLevel


switchScope :: String -> Int -> Evaluator Int
switchScope name newScopeLevel = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            symTab' = symTab { scopeLevels = M.insert name newScopeLevel scopes }
            in
        (newScopeLevel, symTab')


scopeLimit :: Int
scopeLimit = -1


baseScope :: Int
baseScope = 0
