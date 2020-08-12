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
         allTypes,
         parameterDeclared
        ) where


import           Data.Function         (on)
import           Data.List             (sortBy)
import qualified Data.Map              as M

import qualified State.FrameStack      as FrameStack (currentFunc)
import           State.FuncStateAccess (getFuncState, setFuncState)
import           State.GenState        (GenState, throwError)
import           State.SymbolTable     (FuncState (..), ParamVar (..))
import qualified State.SymbolTable     as SymbolTable (mkParVar)
import           Types.Error           (CompilerError (StateError),
                                        StateError (..))
import           Types.Type            (Type)
import           Types.Variables       (VarValue (..))


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
setParamValue name varValue = do
        paramVar <- getParamVar name
        case paramVar of
             Nothing -> throwError $ StateError (NoStateFound name)
             Just pv -> setParamVar name $ pv { paramValue = varValue }


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


getParamVar :: String -> GenState (Maybe ParamVar)
getParamVar paramName = do
        funcName <- FrameStack.currentFunc
        fstate   <- getFuncState funcName
        pure $ M.lookup paramName . parameters $ fstate


setParamVar :: String -> ParamVar -> GenState ()
setParamVar name paramVar = do
        funcName <- FrameStack.currentFunc
        fstate   <- getFuncState funcName
        let fstate' = fstate { parameters = M.insert name paramVar . parameters $ fstate }
        setFuncState funcName fstate'


paramData :: ParamVar -> (Int, Type)
paramData pv = (paramNum pv, paramType pv)


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing
