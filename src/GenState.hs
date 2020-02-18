{-|
Module       : GenState
Description  : State for the code generator

State holder for the code generation stage of compilation.
-}
module GenState
        (GenState,
         runGenState,
         throwError,
         getGlobalScope,
         putGlobalScope,
         getFrameStack,
         putFrameStack,
         startState,
         labelNum,
         getFuncState,
         updateFuncState,
         delFuncState,
         SuccState.getState
        ) where


import qualified Data.Map    as M

import           Error       (CompilerError)
import           SuccState   (SuccStateM, throwError)
import qualified SuccState   (getState, putState, runSuccState)
import           SymbolTable (FuncState, GlobalScope, Stack, SymTab (..),
                              mkSymTab)


-- | State definition
type GenState = SuccStateM SymTab


-- | State constructor
startState :: SymTab
startState = mkSymTab


-- | Run the state extracting the error or result
runGenState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runGenState f t s = SuccState.runSuccState f t s


-- | Get the global scope state holder
getGlobalScope :: GenState GlobalScope
getGlobalScope = do
        state <- SuccState.getState
        pure . globalScope $ state


-- | Update the global scope state holder
putGlobalScope :: GlobalScope -> GenState ()
putGlobalScope gs = do
        state <- SuccState.getState
        SuccState.putState $ state { globalScope = gs }


-- | Get the state for the named function
getFuncState :: String -> GenState (Maybe FuncState)
getFuncState name = M.lookup name . funcStates <$> SuccState.getState


-- | Update function state for named function
updateFuncState :: String -> FuncState -> GenState ()
updateFuncState n s = do
        st <- SuccState.getState
        SuccState.putState $ st { funcStates = M.insert n s $ funcStates st }


-- | Delete function state for named function
delFuncState :: String -> GenState ()
delFuncState n = do
        st <- SuccState.getState
        SuccState.putState $ st { funcStates = M.delete n $ funcStates st }


-- | Get the framestack
getFrameStack :: GenState (Stack String)
getFrameStack = do
        state <- SuccState.getState
        pure . frameStack $ state


-- | Update the framestack
putFrameStack :: Stack String -> GenState ()
putFrameStack stack = do
        state <- SuccState.getState
        SuccState.putState $ state { frameStack = stack }


-- | Get label number, incrementing the state
labelNum :: GenState Int
labelNum = do
        l <- getLabel
        putLabel . succ $ l
        pure l


getLabel :: GenState Int
getLabel = do
        state <- SuccState.getState
        pure . label $ state


putLabel :: Int -> GenState ()
putLabel n = do
        state <- SuccState.getState
        SuccState.putState $ state { label = n }
