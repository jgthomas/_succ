
module Scope (initScope,
              closeScope,
              functionDefined,
              getBreak,
              setBreak,
              getContinue,
              setContinue,
              checkVariable,
              variableOffset,
              newScopeRecord,
              newFuncScopesData,
              storeVar) where


import qualified Data.Map as M

import SimpleStack (currentFunction)
import Evaluator (Evaluator(Ev))
import Types (SymTab(scopeLevels, scopesData),
              LocalScope,
              FunctionScope,
              ProgramScope)


{- API -}

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


getBreak :: Evaluator Int
getBreak = do
        getOffset "@Break"


getContinue :: Evaluator Int
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
        return $ checkVar varName locScope


variableOffset :: String -> Evaluator Int
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

getOffset :: String -> Evaluator Int
getOffset name = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        findOffset currFuncName scopeLevel name


findOffset :: String -> Int -> String -> Evaluator Int
findOffset func scope name =
        if scope == notFound
           then return notFound
           else do
                   offset <- lookUp func scope name
                   if offset == notFound
                      then findOffset func (pred scope) name
                      else return offset


lookUp :: String -> Int -> String -> Evaluator Int
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


checkVar :: String -> LocalScope -> Bool
checkVar varName varMap =
        case M.lookup varName varMap of
             Just v  -> True
             Nothing -> False


getVar :: String -> LocalScope -> Int
getVar varName varMap =
        let value = M.lookup varName varMap
            in case value of
                    Just v  -> v
                    Nothing -> notFound


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


notFound :: Int
notFound = -1

baseScope :: Int
baseScope = 0
