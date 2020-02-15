
module SymTabLocalShared where


import           Error         (CompilerError (StateError),
                                StateError (NoStateFound))
import           GenState      (GenState, throwError)
import qualified GenState      (getFuncState, updateFuncState)
import           GenStateLocal (FuncState)


getFunctionState :: String -> GenState FuncState
getFunctionState name = do
        fstate <- GenState.getFuncState name
        case fstate of
             Just st -> pure st
             Nothing -> throwError $ StateError (NoStateFound name)


setFunctionState :: String -> FuncState -> GenState ()
setFunctionState name fstate = GenState.updateFuncState name fstate
