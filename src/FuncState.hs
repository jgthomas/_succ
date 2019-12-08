
module FuncState (initScope,
                  closeScope,
                  initFunction,
                  closeFunction,
                  getBreak,
                  setBreak,
                  getContinue,
                  setContinue,
                  checkVariable,
                  variableOffset,
                  variableType,
                  addParameter,
                  parameterPosition,
                  parameterType,
                  parameterDeclared,
                  addVariable,
                  delFuncState,
                  allTypes,
                  stackPointerValue) where


import Data.Maybe            (fromMaybe)
import Data.Function         (on)
import Data.List             (sortBy)
import Control.Monad         (unless)
import qualified Data.Map as M

import Evaluator             (Evaluator(Ev))
import Types                 (SymTab(funcStates),
                              FuncState(..),
                              LocalVar(..),
                              ParamVar(..),
                              Type(Label))
import qualified FrameStack  (currentFunction,
                              popFunctionName,
                              pushFunctionName)


initFunction :: String -> Evaluator ()
initFunction name = do
        FrameStack.pushFunctionName name
        check <- checkFunctionState name
        unless check $ newFuncState name


closeFunction :: Evaluator ()
closeFunction = FrameStack.popFunctionName


initScope :: Evaluator ()
initScope = do
        currFuncName  <- FrameStack.currentFunction
        newScopeLevel <- incrementScope
        addNestedScope currFuncName newScopeLevel


closeScope :: Evaluator Int
closeScope = decrementScope


delFuncState :: String -> Evaluator ()
delFuncState name = delFunctionState name


getBreak :: Evaluator (Maybe Int)
getBreak = getOffset "@Break"


getContinue :: Evaluator (Maybe Int)
getContinue = getOffset "@Continue"


setBreak :: Int -> Evaluator ()
setBreak labelNo = store "@Break" labelNo Label


setContinue :: Int -> Evaluator ()
setContinue labelNo = store "@Continue" labelNo Label


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currFuncName <- FrameStack.currentFunction
        scopeLevel   <- findScope currFuncName
        test         <- extract locOffset <$> getLocalVar currFuncName scopeLevel varName
        case test of
             Just _  -> return True
             Nothing -> return False


variableOffset :: String -> Evaluator (Maybe Int)
variableOffset name = getOffset name


variableType :: String -> Evaluator (Maybe Type)
variableType name = getType name


addVariable :: String -> Type -> Evaluator Int
addVariable varName typ = do
        currOff <- currentOffset
        store varName currOff typ
        incrementOffset
        return currOff


stackPointerValue :: Evaluator Int
stackPointerValue = negate <$> currentOffset


addParameter :: String -> Type -> Evaluator ()
addParameter paramName typ = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let funcState' = addParam paramName typ funcState
        setFunctionState currFuncName funcState'


parameterPosition :: String -> Evaluator (Maybe Int)
parameterPosition paramName = do
        currFuncName <- FrameStack.currentFunction
        if currFuncName == "global"
           then return Nothing
           else extract paramNum
                    . M.lookup paramName
                    . parameters <$> getFunctionState currFuncName


parameterType :: String -> Evaluator (Maybe Type)
parameterType paramName = do
        currFuncName <- FrameStack.currentFunction
        extract paramType
            . M.lookup paramName
            . parameters <$> getFunctionState currFuncName


allTypes :: String -> Evaluator [Type]
allTypes funcName = do
        paramList <- M.elems . parameters <$> getFunctionState funcName
        return $ snd <$> sortBy (compare `on` fst) (paramData <$> paramList)


parameterDeclared :: String -> Evaluator Bool
parameterDeclared paramName = do
        pos <- parameterPosition paramName
        case pos of
             Just _  -> return True
             Nothing -> return False


-- store and lookup

getOffset :: String -> Evaluator (Maybe Int)
getOffset varName = getAttr varName locOffset


getType :: String -> Evaluator (Maybe Type)
getType varName = getAttr varName locType


getAttr :: String -> (LocalVar -> a) -> Evaluator (Maybe a)
getAttr varName f = do
        currFuncName <- FrameStack.currentFunction
        if currFuncName == "global"
           then return Nothing
           else do
                   scopeLevel <- findScope currFuncName
                   extract f <$> find currFuncName scopeLevel varName


find :: String -> Int -> String -> Evaluator (Maybe LocalVar)
find funcName scope name =
        if scope == scopeLimit
           then return Nothing
           else do
                   locVar <- getLocalVar funcName scope name
                   case locVar of
                        Nothing  -> find funcName (pred scope) name
                        Just lv  -> return (Just lv)


store :: String -> Int -> Type -> Evaluator ()
store name value typ = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let level      = currentScope funcState
            scope      = getScope level funcState
            locVar     = newLocalVar value typ
            scope'     = M.insert name locVar scope
            funcState' = funcState { scopes = M.insert level scope' $ scopes funcState }
        setFunctionState currFuncName funcState'


newLocalVar :: Int -> Type -> LocalVar
newLocalVar n t = LocVar n t


getLocalVar :: String -> Int -> String -> Evaluator (Maybe LocalVar)
getLocalVar funcName lev var =
        M.lookup var . getScope lev <$> getFunctionState funcName


getScope :: Int -> FuncState -> M.Map String LocalVar
getScope scope fs = fromMaybe
                    (error "scope not defined")
                    (M.lookup scope $ scopes fs)


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


-- FuncState

newFuncState :: String -> Evaluator ()
newFuncState name = Ev $ \symTab ->
        ((), symTab { funcStates = M.insert name makeFs $ funcStates symTab })


makeFs :: FuncState
makeFs = Fs 0 memOffsetSize 0 M.empty (M.singleton 0 M.empty)


addNestedScope :: String -> Int -> Evaluator ()
addNestedScope name level = do
        fs <- getFunctionState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFunctionState name fs'


checkFunctionState :: String -> Evaluator Bool
checkFunctionState n = Ev $ \symTab ->
        case M.lookup n $ funcStates symTab of
             Just _  -> (True, symTab)
             Nothing -> (False, symTab)


getFunctionState :: String -> Evaluator FuncState
getFunctionState n = Ev $ \symTab ->
        case M.lookup n $ funcStates symTab of
             Just st -> (st, symTab)
             Nothing -> error $ "No state defined for: " ++ n


setFunctionState :: String -> FuncState -> Evaluator ()
setFunctionState n st = Ev $ \symTab ->
        ((), symTab { funcStates = M.insert n st $ funcStates symTab })


delFunctionState :: String -> Evaluator ()
delFunctionState n = Ev $ \symTab ->
        ((), symTab { funcStates = M.delete n . funcStates $ symTab })


-- offset

currentOffset :: Evaluator Int
currentOffset = do
        currFuncName <- FrameStack.currentFunction
        funcOffset <$> getFunctionState currFuncName


incrementOffset :: Evaluator ()
incrementOffset = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let funcState' = funcState { funcOffset = funcOffset funcState + memOffsetSize }
        setFunctionState currFuncName funcState'


memOffsetSize :: Int
memOffsetSize = -8


-- parameters

newParamVar :: Int -> Type -> ParamVar
newParamVar n t = ParVar n t


addParam :: String -> Type -> FuncState -> FuncState
addParam n t st =
        let pos = paramCount st
            st' = st { paramCount = succ pos }
            parVar = newParamVar pos t
            in
        st' { parameters = M.insert n parVar $ parameters st' }


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)

