{-|
Module       : FuncState
Description  : Control state of each function

Functions to manipulate the state stored for each function being
compiled.
-}
module FuncState
        (module FuncStateOffset,
         module FuncStateVars,
         module FuncStateScope,
         initFunction,
         closeFunction,
         delFuncState
        ) where


import           Control.Monad   (when)
import           Data.Maybe      (isNothing)

import qualified FrameStack      (popFunc, pushFunc)
import           FuncStateAccess (delFuncState, grabFuncState, setFuncState)
import           FuncStateOffset (incrementOffsetByN, stackPointerValue)
import           FuncStateScope  (closeScope, initScope)
import           FuncStateVars
import           GenState        (GenState)
import qualified SymbolTable     (mkFuncState)


-- | Switch to scope of named function, creating if needed
initFunction :: String -> GenState ()
initFunction name = do
        FrameStack.pushFunc name
        fstate <- grabFuncState name
        when (isNothing fstate) $
            setFuncState name SymbolTable.mkFuncState


-- | Close current function scope
closeFunction :: GenState ()
closeFunction = FrameStack.popFunc
