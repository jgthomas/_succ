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
         module State.FuncStateParams,
         delFuncState
        ) where


import State.FuncStateAccess (delFuncState)
import State.FuncStateOffset (incrementOffsetByN, stackPointerValue)
import State.FuncStateParams (addParameter, allTypes, paramValuesFromArgs,
                              parameterDeclared, parameterPosition,
                              parameterType)
import State.FuncStateScope  (closeFunction, closeScope, initFunction,
                              initScope)
import State.FuncStateVars   (addVariable, checkVariable, getBreak, getContinue,
                              setBreak, setContinue, setLocalValue,
                              variableOffset, variableType)
