
module Scope (getLocalScope,
              getFunctionScope,
              getProgramScope,
              storeVariable,
              updateFunctionScope,
              updateProgramScope,
              checkVar,
              getVar) where


import qualified Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (SymTab(scopesData), LocalScope, FunctionScope, ProgramScope)


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


notFound :: Int
notFound = -1
