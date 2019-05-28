
module FunctionState (newFuncState,
                      getFunctionState,
                      setFunctionState,
                      addParam,
                      paramPos,
                      incrementArgCount,
                      resetArgs) where


import qualified Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (SymTab(funcStates), FuncState(..), FuncStates(..))


{- API -}

newFuncState :: String -> Evaluator String
newFuncState funcName = Ev $ \symTab ->
        let states = funcStates symTab
            symTab' = symTab { funcStates = M.insert funcName makeFuncState states }
            in
        (funcName, symTab')


getFunctionState :: String -> Evaluator FuncState
getFunctionState funcName = Ev $ \symTab ->
        let states = funcStates symTab
            in
        case M.lookup funcName states of
             Just state -> (state, symTab)
             Nothing    -> error $ "No state defined for: " ++ funcName


setFunctionState :: String -> FuncState -> Evaluator FuncStates
setFunctionState funcName funcState = Ev $ \symTab ->
        let states = funcStates symTab
            symTab' = symTab { funcStates = M.insert funcName funcState states }
            states' = funcStates symTab
            in
        (states', symTab')


addParam :: String -> FuncState -> Evaluator FuncState
addParam paramName funcState =
        let params = parameters funcState
            pos = paramCount funcState
            funcState' = funcState { paramCount = pos + 1 }
            funcState'' = funcState' { parameters = M.insert paramName pos params }
            in
        return funcState''


paramPos :: String -> FuncState -> Evaluator Int
paramPos paramName funcState =
        let params = parameters funcState
            in
        case M.lookup paramName params of
             Just pos -> return pos
             Nothing  -> return notFound


incrementArgCount :: FuncState -> Evaluator FuncState
incrementArgCount funcState =
        let count = argCount funcState
            funcState' = funcState { argCount = count + 1 }
            in
        return funcState'


resetArgs :: FuncState -> Evaluator FuncState
resetArgs funcState =
        let funcState' = funcState { argCount = 0 }
            in
        return funcState'


{- Internal -}

makeFuncState :: FuncState
makeFuncState = Fs 0 0 M.empty

notFound :: Int
notFound = -1
