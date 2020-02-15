{-|
Module       : SymTabLocal
Description  : Control state of each function

Functions to manipulate the state stored for each function being
compiled.
-}
module SymTabLocal
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
         module SymTabLocalOffset
        ) where


import           Control.Monad     (when)
import           Data.Function     (on)
import           Data.List         (sortBy)
import qualified Data.Map          as M
import           Data.Maybe        (isNothing)

import           Error             (CompilerError (StateError), StateError (..))
import qualified FrameStack        (currentFunc, popFunc, pushFunc)
import           GenState          (GenState, throwError)
import qualified GenState          (getFuncState)
import           GenStateLocal     (FuncState (..), LocalVar (..),
                                    ParamVar (..))
import qualified GenStateLocal     (mkFuncState, mkLocVar, mkParVar)
import           SymTabLocalOffset
import           SymTabLocalShared (delFuncState, getFuncState, setFuncState)
import           Type              (Type (Label))


-- | Switch to scope of named function, creating if needed
initFunction :: String -> GenState ()
initFunction name = do
        FrameStack.pushFunc name
        fstate <- GenState.getFuncState name
        when (isNothing fstate) $
            setFuncState name GenStateLocal.mkFuncState


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
        incrementOffsetByN 1
        pure currOff


-- | Add a new parameter to the state of a function
addParameter :: String -> Type -> GenState ()
addParameter paramName typ = do
        currFuncName <- FrameStack.currentFunc
        funcState    <- getFuncState currFuncName
        let funcState' = addParam paramName typ funcState
        setFuncState currFuncName funcState'


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
            . parameters <$> getFuncState currFuncName


-- | Retrieve list of all the type of function parameters
allTypes :: String -> GenState [Type]
allTypes funcName = do
        paramList <- M.elems . parameters <$> getFuncState funcName
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
        fstate   <- getFuncState funcName
        let level = currentScope fstate
        scope <- getScope level fstate
        let locVar  = GenStateLocal.mkLocVar value typ
            scope'  = M.insert name locVar scope
            fstate' = fstate { scopes = M.insert level scope' $ scopes fstate }
        setFuncState funcName fstate'


getLocalVar :: String -> Int -> String -> GenState (Maybe LocalVar)
getLocalVar funcName lev var = do
        fstate <- getFuncState funcName
        M.lookup var <$> getScope lev fstate


getScope :: Int -> FuncState -> GenState (M.Map String LocalVar)
getScope scope fs =
        case M.lookup scope $ scopes fs of
             Just sc -> pure sc
             Nothing -> do
                     funcName <- FrameStack.currentFunc
                     throwError $ StateError (UndefinedScope funcName scope)


-- scope

incrementScope :: GenState Int
incrementScope = stepScope succ


decrementScope :: GenState Int
decrementScope = stepScope pred


findScope :: String -> GenState Int
findScope name = currentScope <$> getFuncState name


stepScope :: (Int -> Int) -> GenState Int
stepScope f = do
        funcName  <- FrameStack.currentFunc
        funcState <- getFuncState funcName
        let newLevel = f . currentScope $ funcState
        setFuncState funcName funcState { currentScope = newLevel }
        pure newLevel


-- FuncState

addNestedScope :: String -> Int -> GenState ()
addNestedScope name level = do
        fs <- getFuncState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFuncState name fs'


-- parameters

getParamPos :: String -> String -> GenState (Maybe Int)
getParamPos _ "global" = pure Nothing
getParamPos paramName funcName =
        extract paramNum
        . M.lookup paramName
        . parameters <$> getFuncState funcName


addParam :: String -> Type -> FuncState -> FuncState
addParam name typ fstate =
        let parVar  = GenStateLocal.mkParVar (paramCount fstate) typ
            fstate' = fstate { paramCount = succ . paramCount $ fstate }
            in
        fstate' { parameters = M.insert name parVar . parameters $ fstate' }


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)

