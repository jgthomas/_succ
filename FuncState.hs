
module FuncState (initScope,
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
                  addParameter,
                  parameterPosition,
                  parameterDeclared,
                  memOffsetSize,
                  addVariable,
                  stackPointerValue) where

import qualified Data.Map as M

import Evaluator            (Evaluator(Ev))
import Types                (SymTab(funcStates, offset), FuncState(..))
import qualified FrameStack (currentFunction, popFunctionName, pushFunctionName)


initFunction :: String -> Evaluator ()
initFunction name = do
        FrameStack.pushFunctionName name
        newFuncState name


closeFunction :: Evaluator ()
closeFunction = do
        FrameStack.popFunctionName


initScope :: Evaluator ()
initScope = do
        currFuncName  <- FrameStack.currentFunction
        newScopeLevel <- incrementScope
        addNestedScope currFuncName newScopeLevel


closeScope :: Evaluator Int
closeScope = decrementScope


functionDefined :: String -> Evaluator Bool
functionDefined funcName = checkFunctionState funcName


getBreak :: Evaluator (Maybe Int)
getBreak = getOffset "@Break"


getContinue :: Evaluator (Maybe Int)
getContinue = getOffset "@Continue"


setBreak :: Int -> Evaluator ()
setBreak labelNo = store "@Break" labelNo


setContinue :: Int -> Evaluator ()
setContinue labelNo = store "@Continue" labelNo


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currFuncName <- FrameStack.currentFunction
        scopeLevel   <- findScope currFuncName
        test         <- find currFuncName scopeLevel varName
        case test of
             Just off -> return True
             Nothing  -> return False


variableOffset :: String -> Evaluator (Maybe Int)
variableOffset name = getOffset name


addVariable :: String -> Evaluator Int
addVariable varName = do
        currOff <- currentOffset
        store varName currOff
        incrementOffset
        return currOff


stackPointerValue :: Evaluator Int
stackPointerValue = negate <$> currentOffset


addParameter :: String -> Evaluator ()
addParameter paramName = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let funcState' = addParam paramName funcState
        setFunctionState currFuncName funcState'


parameterPosition :: String -> Evaluator (Maybe Int)
parameterPosition paramName = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        return $ M.lookup paramName $ parameters funcState


parameterDeclared :: String -> Evaluator Bool
parameterDeclared paramName = do
        pos <- parameterPosition paramName
        case pos of
             Just pos -> return True
             Nothing  -> return False


-- store and lookup

getOffset :: String -> Evaluator (Maybe Int)
getOffset name = do
        currFuncName <- FrameStack.currentFunction
        scopeLevel   <- findScope currFuncName
        findOffset currFuncName scopeLevel name


findOffset :: String -> Int -> String -> Evaluator (Maybe Int)
findOffset func scope name =
        if scope == scopeLimit
           then return Nothing
           else do
                   offset <- find func scope name
                   case offset of
                        Nothing  -> findOffset func (pred scope) name
                        Just off -> return (Just off)


store :: String -> Int -> Evaluator ()
store name value = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let level      = currentScope funcState
            scope      = getScope level funcState
            scope'     = M.insert name value scope
            funcState' = funcState { scopes = M.insert level scope' $ scopes funcState }
        setFunctionState currFuncName funcState'


find :: String -> Int -> String -> Evaluator (Maybe Int)
find func level var = do
        funcState <- getFunctionState func
        let scope = getScope level funcState
        return $ M.lookup var scope


getScope :: Int -> FuncState -> M.Map String Int
getScope scope fs =
        case M.lookup scope $ scopes fs of
             Just s  -> s
             Nothing -> error "scope not defined"


-- scope

incrementScope :: Evaluator Int
incrementScope = stepScope succ


decrementScope :: Evaluator Int
decrementScope = stepScope pred


findScope :: String -> Evaluator Int
findScope name = currentScope <$> getFunctionState name


stepScope :: (Int -> Int) -> Evaluator Int
stepScope f = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let newLevel   = f $ currentScope funcState
            funcState' = funcState { currentScope = newLevel }
        setFunctionState currFuncName funcState'
        return newLevel


scopeLimit :: Int
scopeLimit = -1


baseScope :: Int
baseScope = 0


-- FuncState

newFuncState :: String -> Evaluator ()
newFuncState name = Ev $ \symTab ->
        ((), symTab { funcStates = M.insert name makeFs $ funcStates symTab })


makeFs :: FuncState
makeFs = Fs 0 0 M.empty (M.singleton 0 M.empty)


addNestedScope :: String -> Int -> Evaluator ()
addNestedScope name level = do
        fs <- getFunctionState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFunctionState name fs'


checkFunctionState :: String -> Evaluator Bool
checkFunctionState n = Ev $ \symTab ->
        case M.lookup n $ funcStates symTab of
             Just st -> (True, symTab)
             Nothing -> (False, symTab)


getFunctionState :: String -> Evaluator FuncState
getFunctionState n = Ev $ \symTab ->
        case M.lookup n $ funcStates symTab of
             Just st -> (st, symTab)
             Nothing -> error $ "No state defined for: " ++ n


setFunctionState :: String -> FuncState -> Evaluator ()
setFunctionState n st = Ev $ \symTab ->
        ((), symTab { funcStates = M.insert n st $ funcStates symTab })


addParam :: String -> FuncState -> FuncState
addParam n st =
        let pos = paramCount st
            st' = st { paramCount = succ pos }
            in
        st' { parameters = M.insert n pos $ parameters st' }


-- offset

currentOffset :: Evaluator Int
currentOffset = Ev $ \symTab -> (offset symTab, symTab)


incrementOffset :: Evaluator ()
incrementOffset = Ev $ \symTab ->
        ((), symTab { offset = offset symTab + memOffsetSize })


memOffsetSize :: Int
memOffsetSize = -8
