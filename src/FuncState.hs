
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

import Types                 (FuncState(..),
                              LocalVar(..),
                              ParamVar(..),
                              Type(Label))
import GenState (GenState)
import qualified GenState as GenS (getFuncStates, putFuncStates)
import qualified Types       (mkFS,
                              mkLocVar,
                              mkParVar)
import qualified FrameStack  (currentFunction,
                              popFunctionName,
                              pushFunctionName)


initFunction :: String -> GenState ()
initFunction name = do
        FrameStack.pushFunctionName name
        check <- checkFunctionState name
        unless check $ newFuncState name


closeFunction :: GenState ()
closeFunction = FrameStack.popFunctionName


initScope :: GenState ()
initScope = do
        currFuncName  <- FrameStack.currentFunction
        newScopeLevel <- incrementScope
        addNestedScope currFuncName newScopeLevel


closeScope :: GenState ()
closeScope = do
        _ <- decrementScope
        return ()


delFuncState :: String -> GenState ()
delFuncState name = delFunctionState name


getBreak :: GenState (Maybe Int)
getBreak = getOffset "@Break"


getContinue :: GenState (Maybe Int)
getContinue = getOffset "@Continue"


setBreak :: Int -> GenState ()
setBreak labelNo = store "@Break" labelNo Label


setContinue :: Int -> GenState ()
setContinue labelNo = store "@Continue" labelNo Label


checkVariable :: String -> GenState Bool
checkVariable varName = do
        currFuncName <- FrameStack.currentFunction
        scopeLevel   <- findScope currFuncName
        test         <- extract locOffset <$> getLocalVar currFuncName scopeLevel varName
        case test of
             Just _  -> return True
             Nothing -> return False


variableOffset :: String -> GenState (Maybe Int)
variableOffset name = getOffset name


variableType :: String -> GenState (Maybe Type)
variableType name = getType name


addVariable :: String -> Type -> GenState Int
addVariable varName typ = do
        currOff <- currentOffset
        store varName currOff typ
        incrementOffset
        return currOff


stackPointerValue :: GenState Int
stackPointerValue = negate <$> currentOffset


addParameter :: String -> Type -> GenState ()
addParameter paramName typ = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let funcState' = addParam paramName typ funcState
        setFunctionState currFuncName funcState'


parameterPosition :: String -> GenState (Maybe Int)
parameterPosition paramName = do
        currFuncName <- FrameStack.currentFunction
        if currFuncName == "global"
           then return Nothing
           else extract paramNum
                    . M.lookup paramName
                    . parameters <$> getFunctionState currFuncName


parameterType :: String -> GenState (Maybe Type)
parameterType paramName = do
        currFuncName <- FrameStack.currentFunction
        extract paramType
            . M.lookup paramName
            . parameters <$> getFunctionState currFuncName


allTypes :: String -> GenState [Type]
allTypes funcName = do
        paramList <- M.elems . parameters <$> getFunctionState funcName
        return $ snd <$> sortBy (compare `on` fst) (paramData <$> paramList)


parameterDeclared :: String -> GenState Bool
parameterDeclared paramName = do
        pos <- parameterPosition paramName
        case pos of
             Just _  -> return True
             Nothing -> return False


-- store and lookup

getOffset :: String -> GenState (Maybe Int)
getOffset varName = getAttr varName locOffset


getType :: String -> GenState (Maybe Type)
getType varName = getAttr varName locType


getAttr :: String -> (LocalVar -> a) -> GenState (Maybe a)
getAttr varName f = do
        currFuncName <- FrameStack.currentFunction
        if currFuncName == "global"
           then return Nothing
           else do
                   scopeLevel <- findScope currFuncName
                   extract f <$> find currFuncName scopeLevel varName


find :: String -> Int -> String -> GenState (Maybe LocalVar)
find funcName scope name =
        if scope == scopeLimit
           then return Nothing
           else do
                   locVar <- getLocalVar funcName scope name
                   case locVar of
                        Nothing  -> find funcName (pred scope) name
                        Just lv  -> return (Just lv)


store :: String -> Int -> Type -> GenState ()
store name value typ = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let level      = currentScope funcState
            scope      = getScope level funcState
            locVar     = Types.mkLocVar value typ
            scope'     = M.insert name locVar scope
            funcState' = funcState { scopes = M.insert level scope' $ scopes funcState }
        setFunctionState currFuncName funcState'


getLocalVar :: String -> Int -> String -> GenState (Maybe LocalVar)
getLocalVar funcName lev var =
        M.lookup var . getScope lev <$> getFunctionState funcName


getScope :: Int -> FuncState -> M.Map String LocalVar
getScope scope fs = fromMaybe
                    (error "scope not defined")
                    (M.lookup scope $ scopes fs)


-- scope

incrementScope :: GenState Int
incrementScope = stepScope succ


decrementScope :: GenState Int
decrementScope = stepScope pred


findScope :: String -> GenState Int
findScope name = currentScope <$> getFunctionState name


stepScope :: (Int -> Int) -> GenState Int
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

newFuncState :: String -> GenState ()
newFuncState name = do
        fstates <- GenS.getFuncStates
        GenS.putFuncStates $ M.insert name Types.mkFS fstates


addNestedScope :: String -> Int -> GenState ()
addNestedScope name level = do
        fs <- getFunctionState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFunctionState name fs'


checkFunctionState :: String -> GenState Bool
checkFunctionState name = do
        fstates <- GenS.getFuncStates
        case M.lookup name fstates of
             Just _  -> return True
             Nothing -> return False


getFunctionState :: String -> GenState FuncState
getFunctionState name = do
        fstates <- GenS.getFuncStates
        case M.lookup name fstates of
             Just st -> return st
             Nothing -> error $ "No state defined for: " ++ name


setFunctionState :: String -> FuncState -> GenState ()
setFunctionState name st = do
        fstates <- GenS.getFuncStates
        GenS.putFuncStates $ M.insert name st fstates


delFunctionState :: String -> GenState ()
delFunctionState name = do
        fstates <- GenS.getFuncStates
        GenS.putFuncStates $ M.delete name fstates


-- offset

currentOffset :: GenState Int
currentOffset = do
        currFuncName <- FrameStack.currentFunction
        funcOffset <$> getFunctionState currFuncName


incrementOffset :: GenState ()
incrementOffset = do
        currFuncName <- FrameStack.currentFunction
        funcState    <- getFunctionState currFuncName
        let funcState' = funcState { funcOffset = funcOffset funcState + memOffsetSize }
        setFunctionState currFuncName funcState'


memOffsetSize :: Int
memOffsetSize = -8


-- parameters

addParam :: String -> Type -> FuncState -> FuncState
addParam n t st =
        let pos = paramCount st
            st' = st { paramCount = succ pos }
            parVar = Types.mkParVar pos t
            in
        st' { parameters = M.insert n parVar $ parameters st' }


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)

