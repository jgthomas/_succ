{-|
Module       : FuncStateParams
Description  : Manages parameters

Functions for creating and managing parameter variables.
-}
module State.FuncStateParams
        (addParameter,
         parameterPosition,
         parameterType,
         setParamValue,
         getParamValue,
         allTypes,
         parameterDeclared,
         paramValuesFromArgs,
         updateParameters
        ) where


import           Control.Monad         (unless, when)
import           Data.List             (sortOn)
import qualified Data.Map              as M

import qualified State.FrameStack      as FrameStack (currentFunc, getScope)
import           State.FuncStateAccess (getFuncState, setFuncState)
import           State.GenState        (GenState, throwError)
import           State.SymbolTable     (FuncState (paramCount, parameters, posToParam),
                                        ParamVar (..))
import qualified State.SymbolTable     as SymbolTable (mkParVar)
import           Types.Error           (CompilerError (StateError),
                                        StateError (..))
import           Types.Type            (Type)
import           Types.Variables       (Scope (..), VarValue (..))


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


-- | Set the stored value of the parameter
setParamValue :: String -> VarValue -> GenState ()
setParamValue paramName varValue = do
        scope <- FrameStack.getScope
        unless (scope == Global) $ do
           funcName <- FrameStack.currentFunc
           paramVar <- getParamVar funcName paramName
           case paramVar of
                Nothing -> throwError $ StateError (NoStateFound $ errMsg funcName paramName)
                Just pv -> setParamVar funcName paramName $ pv { paramValue = varValue }


-- | Get the stored value of the parameter
getParamValue :: String -> GenState (Maybe VarValue)
getParamValue paramName = do
        scope <- FrameStack.getScope
        if scope == Global
           then pure Nothing
           else do
                   funcName <- FrameStack.currentFunc
                   paramVar <- getParamVar funcName paramName
                   case paramVar of
                        Nothing -> pure Nothing
                        Just pv ->
                                if paramValue pv == UntrackedValue
                                   then pure (Just $ argValue pv)
                                   else pure (Just $ paramValue pv)


-- | Retrieve list of all the type of function parameters
allTypes :: String -> GenState [Type]
allTypes funcName = map paramType <$> orderedParamVars funcName


-- | Check a parameter exits for function
parameterDeclared :: String -> GenState Bool
parameterDeclared paramName = do
        pos <- parameterPosition paramName
        case pos of
             Just _  -> pure True
             Nothing -> pure False


-- | Set argument values as initial parameter values
paramValuesFromArgs :: String -> [(Int, VarValue)] -> GenState ()
paramValuesFromArgs funcName argList = mapM_ (paramValueFromArg funcName) argList


-- | Update parameter data on function redeclaration
updateParameters :: [(Int, String)] -> GenState ()
updateParameters posAndName = mapM_ updateParameter posAndName


updateParameter :: (Int, String) -> GenState ()
updateParameter (pos, newName) = do
        prevName <- previousName pos
        when (prevName /= newName) $ do
            updateParamName prevName newName
            updateParamPos pos newName


previousName :: Int -> GenState String
previousName pos = do
        currFuncName <- FrameStack.currentFunc
        nameAtPos    <- M.lookup pos . posToParam <$> getFuncState currFuncName
        case nameAtPos of
             Nothing   -> throwError $ StateError (NoStateFound $ errMsg currFuncName (show pos))
             Just name -> pure name


updateParamName :: String -> String -> GenState ()
updateParamName prevName newName = do
        currFuncName  <- FrameStack.currentFunc
        fstate        <- getFuncState currFuncName
        let prevData = M.lookup prevName . parameters $ fstate
        case prevData of
             Nothing  -> throwError $ StateError (NoStateFound $ errMsg currFuncName prevName)
             Just dat -> do
                     let fstate'  = fstate { parameters = M.delete prevName . parameters $ fstate }
                         fstate'' = fstate' { parameters = M.insert newName dat . parameters $ fstate' }
                     setFuncState currFuncName  fstate''


updateParamPos :: Int -> String -> GenState ()
updateParamPos pos newName = do
        currFuncName <- FrameStack.currentFunc
        fstate       <- getFuncState currFuncName
        let fstate' = fstate { posToParam = M.insert pos newName . posToParam $ fstate }
        setFuncState currFuncName fstate'


paramValueFromArg :: String -> (Int, VarValue) -> GenState ()
paramValueFromArg funcName (pos, varValue) = do
        paramName <- parameterNameFromPosition funcName pos
        case paramName of
             Nothing -> throwError $ StateError (NoStateFound $ errMsg funcName (show pos))
             Just pn -> setParamVarFromArg funcName pn varValue


setParamVarFromArg :: String -> String -> VarValue -> GenState ()
setParamVarFromArg funcName paramName varValue = do
        paramVar <- getParamVar funcName paramName
        case paramVar of
             Nothing -> throwError $ StateError (NoStateFound $ errMsg funcName paramName)
             Just pv -> setParamVar funcName paramName $ pv { argValue = varValue }


parameterNameFromPosition :: String -> Int -> GenState (Maybe String)
parameterNameFromPosition funcName pos =
        M.lookup pos . posToParam <$> getFuncState funcName


getParamPos :: String -> String -> GenState (Maybe Int)
getParamPos _ "global" = pure Nothing
getParamPos paramName funcName =
        extract paramNum
        . M.lookup paramName
        . parameters <$> getFuncState funcName


addParam :: String -> Type -> FuncState -> FuncState
addParam name typ fstate =
        let paramPos = paramCount fstate
            parVar   = SymbolTable.mkParVar paramPos typ
            fstate'  = fstate { posToParam = M.insert paramPos name $ posToParam fstate }
            fstate'' = fstate' { paramCount = succ paramPos }
            in
        fstate'' { parameters = M.insert name parVar . parameters $ fstate'' }


getParamVar :: String -> String -> GenState (Maybe ParamVar)
getParamVar funcName paramName = do
        fstate <- getFuncState funcName
        pure $ M.lookup paramName . parameters $ fstate


setParamVar :: String -> String -> ParamVar -> GenState ()
setParamVar funcName paramName paramVar = do
        fstate <- getFuncState funcName
        let fstate' = fstate
                      { parameters = M.insert paramName paramVar . parameters $ fstate }
        setFuncState funcName fstate'


orderedParamVars :: String -> GenState [ParamVar]
orderedParamVars funcName = sortOn paramNum . M.elems . parameters <$> getFuncState funcName


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


errMsg :: String -> String -> String
errMsg funcName paramName = "Function: " ++ funcName ++ ", Parameter: " ++ paramName
