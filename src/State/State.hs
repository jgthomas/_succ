{-|
Module       : State
Description  : Shared state querying functions

Functions that query state in local and global scope
-}
module State.State
        (module State.FrameStack,
         SymTab,
         labelNum,
         memOffset,
         getVariable,
         setVariableValue,
         getVariableValue
        ) where


import           State.FrameStack  (currentFunc, getScope)
import qualified State.FuncState   (getLocalValue, getParamValue,
                                    parameterPosition, setLocalValue,
                                    setParamValue, variableOffset)
import           State.GenState    (GenState, labelNum, throwError)
import qualified State.GlobalState (getLabel, getValue, setValue)
import           State.SymbolTable (SymTab, memOffset)
import           Types.Error       (CompilerError (StateError), StateError (..))
import           Types.Variables   (VarLookup (..), VarType (..), VarValue (..))


-- | Build variable data type from retrieved data
getVariable :: String -> GenState VarLookup
getVariable varName = do
        localVar  <- mkVarLocal <$> State.FuncState.variableOffset varName
        paramVar  <- mkVarParam <$> State.FuncState.parameterPosition varName
        globalVar <- mkVarGlobal <$> State.GlobalState.getLabel varName
        case (localVar, paramVar, globalVar) of
             (var@(VarType LocalVar{}), _, _)  -> pure var
             (_, var@(VarType ParamVar{}), _)  -> pure var
             (_, _, var@(VarType GlobalVar{})) -> pure var
             (_, _, _)                         -> pure NotFound


-- | Retrieve the value of a variable
getVariableValue :: String -> GenState VarValue
getVariableValue varName = do
        locValue    <- State.FuncState.getLocalValue varName
        paramValue  <- State.FuncState.getParamValue varName
        globalValue <- State.GlobalState.getValue varName
        case (locValue, paramValue, globalValue) of
             (Just lv, _, _) -> pure lv
             (_, Just pv, _) -> pure pv
             (_, _, Just gv) -> pure gv
             (_, _, _)       -> throwError $ StateError (NoStateFound $ errMsg varName)


-- | Store the value of a variable
setVariableValue :: String -> VarValue -> GenState ()
setVariableValue varName varValue = do
        localVar  <- State.FuncState.variableOffset varName
        paramVar  <- State.FuncState.parameterPosition varName
        globalVar <- State.GlobalState.getLabel varName
        case (localVar, paramVar, globalVar) of
             (Just _, _, _) -> State.FuncState.setLocalValue varName varValue
             (_, Just _, _) -> State.FuncState.setParamValue varName varValue
             (_, _, Just _) -> State.GlobalState.setValue varName varValue
             (_, _, _)      -> throwError $ StateError (NoStateFound $ errMsg varName)


mkVarLocal :: Maybe Int -> VarLookup
mkVarLocal (Just n) = VarType (LocalVar n 0 (-n))
mkVarLocal Nothing  = NotFound


mkVarParam :: Maybe Int -> VarLookup
mkVarParam (Just n) = VarType (ParamVar n 0)
mkVarParam Nothing  = NotFound


mkVarGlobal :: Maybe String -> VarLookup
mkVarGlobal (Just s) = VarType (GlobalVar s 0)
mkVarGlobal Nothing  = NotFound


errMsg :: String -> String
errMsg varName = "Variable: " ++ varName
