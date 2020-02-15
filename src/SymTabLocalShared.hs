
module SymTabLocalShared where


import           Error         (CompilerError (StateError),
                                StateError (NoStateFound))
import           GenState      (GenState, throwError)
import qualified GenState      (delFuncState, getFuncState, updateFuncState)
import           GenStateLocal (FuncState)


getFuncState :: String -> GenState FuncState
getFuncState name = do
        fstate <- GenState.getFuncState name
        case fstate of
             Just st -> pure st
             Nothing -> throwError $ StateError (NoStateFound name)


setFuncState :: String -> FuncState -> GenState ()
setFuncState name fstate = GenState.updateFuncState name fstate


-- | Delete named function state record
delFuncState :: String -> GenState ()
delFuncState name = GenState.delFuncState name
