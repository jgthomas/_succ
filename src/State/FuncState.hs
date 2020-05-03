{-|
Module       : FuncState
Description  : Control state of each function

Functions to manipulate the state stored for each function being
compiled.
-}
module State.FuncState
        (module State.FuncStateOffset,
         module State.FuncStateVars,
         module State.FuncStateScope,
         initFunction,
         closeFunction,
         delFuncState
        ) where


import           Control.Monad         (when)
import           Data.Maybe            (isNothing)

import           GenState              (GenState)
import qualified State.FrameStack      as FrameStack (popFunc, pushFunc)
import           State.FuncStateAccess (delFuncState, grabFuncState,
                                        setFuncState)
import           State.FuncStateOffset (incrementOffsetByN, stackPointerValue)
import           State.FuncStateScope  (closeScope, initScope)
import           State.FuncStateVars
import qualified State.SymbolTable     as SymbolTable (mkFuncState)


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
