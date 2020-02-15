{-|
Module       : SymTabLocal
Description  : Control state of each function

Functions to manipulate the state stored for each function being
compiled.
-}
module SymTabLocal
        (module SymTabLocalOffset,
         module SymTabLocalVars,
         module SymTabLocalScope,
         initFunction,
         closeFunction,
         delFuncState
        ) where


import           Control.Monad     (when)
import           Data.Maybe        (isNothing)

import qualified FrameStack        (popFunc, pushFunc)
import           GenState          (GenState)
import qualified GenState          (getFuncState)
import qualified GenStateLocal     (mkFuncState)
import           SymTabLocalOffset (stackPointerValue)
import           SymTabLocalScope  (closeScope, initScope)
import           SymTabLocalShared (delFuncState, setFuncState)
import           SymTabLocalVars


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
