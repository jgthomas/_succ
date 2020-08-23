{-|
Module       : FuncStateVars
Description  : Manages local variables

Functions for creating and managing local variables.
-}
module State.FuncStateVars
        (checkVariable,
         variableOffset,
         variableType,
         addVariable,
         getBreak,
         setBreak,
         getContinue,
         setContinue,
         getLocalValue,
         setLocalValue,
        ) where


import           Control.Monad         (unless)
import qualified Data.Map              as M

import qualified State.FrameStack      as FrameStack (currentFunc, getScope)
import           State.FuncStateAccess (getFuncState, setFuncState)
import           State.FuncStateOffset (currentOffset, incrementOffsetByN)
import           State.FuncStateScope  (scopeDepth)
import           State.GenState        (GenState, throwError)
import           State.SymbolTable     (FuncState (scopes), LocalVar (..))
import qualified State.SymbolTable     as SymbolTable (mkLocVar)
import           Types.Error           (CompilerError (StateError),
                                        StateError (..))
import           Types.Type            (Type (Label))
import           Types.Variables       (Scope (..), VarValue (..))


-- | Check if variable name is in use in current scope
checkVariable :: String -> GenState Bool
checkVariable varName = do
        fName  <- FrameStack.currentFunc
        scope  <- scopeDepth fName
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


getLocalValue :: String -> GenState (Maybe VarValue)
getLocalValue name = do
        scope <- FrameStack.getScope
        if scope == Global
           then pure Nothing
           else getAttribute locValue name


setLocalValue :: String -> VarValue -> GenState ()
setLocalValue varName varValue = do
        scope <- FrameStack.getScope
        unless (scope == Global) $ do
            funcName <- FrameStack.currentFunc
            level    <- scopeDepth funcName
            (localVar, varLevel) <- findVarAndScopeLevel funcName varName level
            case localVar of
                 Nothing -> throwError $ StateError (NoStateFound varName)
                 Just lv -> setLocalVar funcName varName varLevel $ lv { locValue = varValue }


-- | Store new variable, returning offset from base pointer
addVariable :: String -> Type -> GenState Int
addVariable varName typ = do
        incrementOffsetByN 1
        currOff <- currentOffset
        store varName currOff typ
        pure currOff


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


getAttribute :: (LocalVar -> a) -> String -> GenState (Maybe a)
getAttribute f varName = do
        funcName <- FrameStack.currentFunc
        getAttr f varName funcName


getAttr :: (LocalVar -> a) -> String -> String -> GenState (Maybe a)
getAttr _ _ "global" = pure Nothing
getAttr f varName funcName = do
        scopeLevel <- scopeDepth funcName
        extract f . fst <$> findVarAndScopeLevel funcName varName scopeLevel


findVarAndScopeLevel :: String -> String -> Int -> GenState (Maybe LocalVar, Int)
findVarAndScopeLevel _ _ (-1) = pure (Nothing, -1)
findVarAndScopeLevel funcName varName scope = do
        locVar <- getLocalVar funcName scope varName
        case locVar of
             Nothing -> findVarAndScopeLevel funcName varName (pred scope)
             Just lv -> pure (Just lv, scope)


store :: String -> Int -> Type -> GenState ()
store varName value typ = do
        funcName <- FrameStack.currentFunc
        level    <- scopeDepth funcName
        setLocalVar funcName varName level $ SymbolTable.mkLocVar value typ


getLocalVar :: String -> Int -> String -> GenState (Maybe LocalVar)
getLocalVar funcName level varName = do
        fstate <- getFuncState funcName
        M.lookup varName <$> getScope level fstate


setLocalVar :: String -> String -> Int -> LocalVar -> GenState ()
setLocalVar funcName varName level localVar = do
        fstate <- getFuncState funcName
        scope  <- getScope level fstate
        let scope'  = M.insert varName localVar scope
            fstate' = fstate { scopes = M.insert level scope' $ scopes fstate }
        setFuncState funcName fstate'


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


getScope :: Int -> FuncState -> GenState (M.Map String LocalVar)
getScope scope fs =
        case M.lookup scope $ scopes fs of
             Just sc -> pure sc
             Nothing -> do
                     funcName <- FrameStack.currentFunc
                     throwError $ StateError (UndefinedScope funcName scope)
