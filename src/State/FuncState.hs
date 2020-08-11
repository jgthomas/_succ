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
         delFuncState
        ) where


import State.FuncStateAccess (delFuncState)
import State.FuncStateOffset (incrementOffsetByN, stackPointerValue)
import State.FuncStateScope  (closeFunction, closeScope, initFunction,
                              initScope)
import State.FuncStateVars   (addParameter, addVariable, allTypes,
                              checkVariable, getBreak, getContinue,
                              parameterDeclared, parameterPosition,
                              parameterType, setBreak, setContinue,
                              setLocalValue, variableOffset, variableType)
