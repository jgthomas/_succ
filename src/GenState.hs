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
         getFuncStates,
         putFuncStates,
         getFrameStack,
         putFrameStack,
         startState,
         labelNum,
         getFuncState
        ) where


import qualified Data.Map    as M

import           Error       (CompilerError)
import           GlobalScope (GlobalScope, mkGlobalScope)
import           LocalScope  (FuncState)
import           Stack       (Stack, mkStack)
import           SuccState   (SuccStateM, throwError)
import qualified SuccState   (getState, putState, runSuccState)


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)


-- | State definition
type GenState = SuccStateM SymTab


-- | State constructor
startState :: SymTab
startState = Tab 1 mkStack mkGlobalScope M.empty


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


-- | Get the function scope state holder
getFuncStates :: GenState (M.Map String FuncState)
getFuncStates = do
        state <- SuccState.getState
        pure . funcStates $ state


-- | Get the state for the named function
getFuncState :: String -> GenState (Maybe FuncState)
getFuncState name = M.lookup name . funcStates <$> SuccState.getState


-- | Update the function scope state holder
putFuncStates :: M.Map String FuncState -> GenState ()
putFuncStates fs = do
        state <- SuccState.getState
        SuccState.putState $ state { funcStates = fs }


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
