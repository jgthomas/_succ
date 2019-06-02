
module FunctionState (newFuncState,
                      addParameter,
                      parameterPosition,
                      parameterDeclared) where


import qualified Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (SymTab(funcStates), FuncState(..), FuncStates(..))
import FrameStack (currentFunction)


{- API -}

newFuncState :: String -> Evaluator String
newFuncState funcName = Ev $ \symTab ->
        let states = funcStates symTab
            symTab' = symTab { funcStates = M.insert funcName makeFuncState states }
            in
        (funcName, symTab')


addParameter :: String -> Evaluator FuncStates
addParameter paramName = do
        currFuncName <- currentFunction
        funcState    <- getFunctionState currFuncName
        funcState'   <- addParam paramName funcState
        setFunctionState currFuncName funcState'


parameterPosition :: String -> Evaluator (Maybe Int)
parameterPosition paramName = do
        paramPosition paramName


parameterDeclared :: String -> Evaluator Bool
parameterDeclared paramName = do
        pos <- paramPosition paramName
        case pos of
             Just pos -> return True
             Nothing  -> return False


{- Internal -}

paramPosition :: String -> Evaluator (Maybe Int)
paramPosition paramName = do
        currFuncName <- currentFunction
        funcState    <- getFunctionState currFuncName
        case M.lookup paramName $ parameters funcState of
             Just pos -> return (Just pos)
             Nothing  -> return Nothing


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


makeFuncState :: FuncState
makeFuncState = Fs 0 M.empty
