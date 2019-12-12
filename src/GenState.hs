
module GenState where


import qualified Data.Map as M

import Types     (SymTab(..),
                  GlobalScope,
                  FuncState,
                  Stack)
import SuccState (SuccStateM,
                  getState,
                  putState)


type GenState = SuccStateM SymTab


getGlobalScope :: GenState GlobalScope
getGlobalScope = do
        state <- getState
        return . globalScope $ state


updateGlobalScope :: GlobalScope -> GenState ()
updateGlobalScope gs = do
        state <- getState
        putState $ state { globalScope = gs }


getFuncStates :: GenState (M.Map String FuncState)
getFuncStates = do
        state <- getState
        return . funcStates $ state


putFuncStates :: M.Map String FuncState -> GenState ()
putFuncStates fs = do
        state <- getState
        putState $ state { funcStates = fs }


getFrameStack :: GenState (Stack String)
getFrameStack = do
        state <- getState
        return . frameStack $ state


putFrameStack :: Stack String -> GenState ()
putFrameStack stack = do
        state <- getState
        putState $ state { frameStack = stack }


getLabel :: GenState Int
getLabel = do
        state <- getState
        return . label $ state


putLabel :: Int -> GenState ()
putLabel n = do
        state <- getState
        putState $ state { label = n }
