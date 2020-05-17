{-|
Module       : FuncStateAccess
Description  : Access function state

Provides access to the state data structure for a particular function, allowing
it to be retrieved, updated, and deleted.
-}
module State.FuncStateAccess where


import           State.GenState    (GenState, throwError)
import qualified State.GenState    as GenState (delFuncState, getFuncState,
                                                updateFuncState)
import           State.SymbolTable (FuncState)
import           Types.Error       (CompilerError (StateError),
                                    StateError (NoStateFound))


-- | Retrieve a named function state record, without checking for errors
grabFuncState :: String -> GenState (Maybe FuncState)
grabFuncState name = GenState.getFuncState name


-- | Retrieve a named function state record, checking for errors
getFuncState :: String -> GenState FuncState
getFuncState name = do
        fstate <- grabFuncState name
        case fstate of
             Just st -> pure st
             Nothing -> throwError $ StateError (NoStateFound name)


-- | Store a named function state record
setFuncState :: String -> FuncState -> GenState ()
setFuncState name fstate = GenState.updateFuncState name fstate


-- | Delete named function state record
delFuncState :: String -> GenState ()
delFuncState name = GenState.delFuncState name
