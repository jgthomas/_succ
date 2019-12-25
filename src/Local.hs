{-|
Module       : Local
Description  : Control state of each function

Functions to manipulate the state stored for each function being
compiled.
-}
module Local
        (initScope,
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
         stackPointerValue
        ) where


import           Control.Monad (when)
import           Data.Function (on)
import           Data.List     (sortBy)
import qualified Data.Map      as M
import           Data.Maybe    (isNothing)

import           Error         (CompilerError (GeneratorError),
                                GeneratorError (..))
import qualified FrameStack    (currentFunc, popFunc, pushFunc)
import           GenState      (GenState, throwError)
import qualified GenState      (delFuncState, getFuncState, updateFuncState)
import           LocalScope    (FuncState (..), LocalVar (..), ParamVar (..))
import qualified LocalScope    (memOffset, mkFuncState, mkLocVar, mkParVar)
import           Type          (Type (Label))


-- | Switch to scope of named function, creating if needed
initFunction :: String -> GenState ()
initFunction name = do
        FrameStack.pushFunc name
        fstate <- GenState.getFuncState name
        when (isNothing fstate) $
            GenState.updateFuncState name LocalScope.mkFuncState


-- | Close current function scope
closeFunction :: GenState ()
closeFunction = FrameStack.popFunc


-- | Initialize a new scope inside a function
initScope :: GenState ()
initScope = do
        currFuncName  <- FrameStack.currentFunc
        newScopeLevel <- incrementScope
        addNestedScope currFuncName newScopeLevel


-- | Exit current scope inside a function
closeScope :: GenState ()
closeScope = do
        _ <- decrementScope
        pure ()


-- | Delete named function state record
delFuncState :: String -> GenState ()
delFuncState name = GenState.delFuncState name


-- | Get the break label number for current scope
getBreak :: GenState (Maybe Int)
getBreak = getOffset "@Break"


-- | Get the continue label number for current scope
getContinue :: GenState (Maybe Int)
getContinue = getOffset "@Continue"


-- | Set the break label number for current scope
setBreak :: Int -> GenState ()
setBreak labelNo = store "@Break" labelNo Label


-- | Set the continue label number for current scope
setContinue :: Int -> GenState ()
setContinue labelNo = store "@Continue" labelNo Label


-- | Check if variable name is in use in current scope
checkVariable :: String -> GenState Bool
checkVariable varName = do
        funcName <- FrameStack.currentFunc
        scope    <- findScope funcName
        check    <- extract locOffset
                    <$>
                    getLocalVar funcName scope varName
        case check of
             Just _  -> pure True
             Nothing -> pure False


-- | Get the offset from base pointer of variable
variableOffset :: String -> GenState (Maybe Int)
variableOffset name = getOffset name


-- | Get the type of variable
variableType :: String -> GenState (Maybe Type)
variableType name = getType name


-- | Store new variable, returning offset from base pointer
addVariable :: String -> Type -> GenState Int
addVariable varName typ = do
        currOff <- currentOffset
        store varName currOff typ
        incrementOffset
        pure currOff


-- | Retrieve current value of stack pointer
stackPointerValue :: GenState Int
stackPointerValue = negate <$> currentOffset


-- | Add a new parameter to the state of a function
addParameter :: String -> Type -> GenState ()
addParameter paramName typ = do
        currFuncName <- FrameStack.currentFunc
        funcState    <- getFunctionState currFuncName
        let funcState' = addParam paramName typ funcState
        setFunctionState currFuncName funcState'


-- | Retrieve the position of function parameter
parameterPosition :: String -> GenState (Maybe Int)
parameterPosition paramName = do
        currFuncName <- FrameStack.currentFunc
        if currFuncName == "global"
           then pure Nothing
           else extract paramNum
                . M.lookup paramName
                . parameters <$> getFunctionState currFuncName


-- | Retrieve the type of function parameter
parameterType :: String -> GenState (Maybe Type)
parameterType paramName = do
        currFuncName <- FrameStack.currentFunc
        extract paramType
            . M.lookup paramName
            . parameters <$> getFunctionState currFuncName


-- | Retrieve list of all the type of function parameters
allTypes :: String -> GenState [Type]
allTypes funcName = do
        paramList <- M.elems . parameters <$> getFunctionState funcName
        pure $ snd <$> sortBy (compare `on` fst) (paramData <$> paramList)


-- | Check a parameter exits for function
parameterDeclared :: String -> GenState Bool
parameterDeclared paramName = do
        pos <- parameterPosition paramName
        case pos of
             Just _  -> pure True
             Nothing -> pure False


-- store and lookup

getOffset :: String -> GenState (Maybe Int)
getOffset varName = getAttr varName locOffset


getType :: String -> GenState (Maybe Type)
getType varName = getAttr varName locType


getAttr :: String -> (LocalVar -> a) -> GenState (Maybe a)
getAttr varName f = do
        currFuncName <- FrameStack.currentFunc
        if currFuncName == "global"
           then pure Nothing
           else do
                   scopeLevel <- findScope currFuncName
                   extract f <$> find currFuncName scopeLevel varName


find :: String -> Int -> String -> GenState (Maybe LocalVar)
find funcName scope name =
        if scope == scopeLimit
           then pure Nothing
           else do
                   locVar <- getLocalVar funcName scope name
                   case locVar of
                        Nothing -> find funcName (pred scope) name
                        Just lv -> pure (Just lv)


store :: String -> Int -> Type -> GenState ()
store name value typ = do
        funcName <- FrameStack.currentFunc
        fstate   <- getFunctionState funcName
        let level = currentScope fstate
        scope <- getScope level fstate
        let locVar  = LocalScope.mkLocVar value typ
            scope'  = M.insert name locVar scope
            fstate' = fstate { scopes = M.insert level scope' $ scopes fstate }
        setFunctionState funcName fstate'


getLocalVar :: String -> Int -> String -> GenState (Maybe LocalVar)
getLocalVar funcName lev var = do
        fstate <- getFunctionState funcName
        scope  <- getScope lev fstate
        pure $ M.lookup var scope


getScope :: Int -> FuncState -> GenState (M.Map String LocalVar)
getScope scope fs =
        case M.lookup scope $ scopes fs of
             Just sc -> pure sc
             Nothing -> throwError $ GeneratorError (UndefinedScope scope)


-- scope

incrementScope :: GenState Int
incrementScope = stepScope succ


decrementScope :: GenState Int
decrementScope = stepScope pred


findScope :: String -> GenState Int
findScope name = currentScope <$> getFunctionState name


stepScope :: (Int -> Int) -> GenState Int
stepScope f = do
        currFuncName <- FrameStack.currentFunc
        funcState    <- getFunctionState currFuncName
        let newLevel   = f $ currentScope funcState
            funcState' = funcState { currentScope = newLevel }
        setFunctionState currFuncName funcState'
        pure newLevel


scopeLimit :: Int
scopeLimit = -1


-- FuncState

addNestedScope :: String -> Int -> GenState ()
addNestedScope name level = do
        fs <- getFunctionState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFunctionState name fs'


getFunctionState :: String -> GenState FuncState
getFunctionState name = do
        fstate <- GenState.getFuncState name
        case fstate of
             Just st -> pure st
             Nothing -> throwError $ GeneratorError (NoStateFound name)


setFunctionState :: String -> FuncState -> GenState ()
setFunctionState name fstate = GenState.updateFuncState name fstate


-- offset

currentOffset :: GenState Int
currentOffset = do
        currFuncName <- FrameStack.currentFunc
        funcOffset <$> getFunctionState currFuncName


incrementOffset :: GenState ()
incrementOffset = do
        funcName <- FrameStack.currentFunc
        fstate   <- getFunctionState funcName
        let offset  = funcOffset fstate
            fstate' = fstate { funcOffset = offset + LocalScope.memOffset }
        setFunctionState funcName fstate'


-- parameters

addParam :: String -> Type -> FuncState -> FuncState
addParam n t st =
        let pos = paramCount st
            st' = st { paramCount = succ pos }
            parVar = LocalScope.mkParVar pos t
            in
        st' { parameters = M.insert n parVar $ parameters st' }


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)

