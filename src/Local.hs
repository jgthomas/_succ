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
getBreak = getAttribute locOffset "@Break"


-- | Get the continue label number for current scope
getContinue :: GenState (Maybe Int)
getContinue = getAttribute locOffset "@Continue"


-- | Set the break label number for current scope
setBreak :: Int -> GenState ()
setBreak labelNo = store "@Break" labelNo Label


-- | Set the continue label number for current scope
setContinue :: Int -> GenState ()
setContinue labelNo = store "@Continue" labelNo Label


-- | Check if variable name is in use in current scope
checkVariable :: String -> GenState Bool
checkVariable varName = do
        fName  <- FrameStack.currentFunc
        scope  <- findScope fName
        locVar <- getLocalVar fName scope varName
        case locVar of
             Just _  -> pure True
             Nothing -> pure False


-- | Get the offset from base pointer of variable
variableOffset :: String -> GenState (Maybe Int)
variableOffset name = getAttribute locOffset name


-- | Get the type of variable
variableType :: String -> GenState (Maybe Type)
variableType name = getAttribute locType name


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
        funcName <- FrameStack.currentFunc
        getParamPos paramName funcName


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

getAttribute :: (LocalVar -> a) -> String -> GenState (Maybe a)
getAttribute f varName = do
        funcName <- FrameStack.currentFunc
        getAttr f varName funcName


getAttr :: (LocalVar -> a) -> String -> String -> GenState (Maybe a)
getAttr _ _ "global" = pure Nothing
getAttr f varName funcName = do
        scopeLevel <- findScope funcName
        extract f <$> find funcName scopeLevel varName


find :: String -> Int -> String -> GenState (Maybe LocalVar)
find _ (-1) _ = pure Nothing
find funcName scope name = do
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
        funcName  <- FrameStack.currentFunc
        funcState <- getFunctionState funcName
        let newLevel = f . currentScope $ funcState
        setFunctionState funcName funcState { currentScope = newLevel }
        pure newLevel


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
        name  <- FrameStack.currentFunc
        fs    <- getFunctionState name
        let fs' = fs { funcOffset = funcOffset fs + LocalScope.memOffset }
        setFunctionState name fs'


-- parameters

getParamPos :: String -> String -> GenState (Maybe Int)
getParamPos _ "global" = pure Nothing
getParamPos paramName funcName =
        extract paramNum
        . M.lookup paramName
        . parameters <$> getFunctionState funcName


addParam :: String -> Type -> FuncState -> FuncState
addParam name typ fstate =
        let parVar  = LocalScope.mkParVar (paramCount fstate) typ
            fstate' = fstate { paramCount = succ . paramCount $ fstate }
            in
        fstate' { parameters = M.insert name parVar . parameters $ fstate' }


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)

