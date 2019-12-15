
module GenState
        (GenState,
         mkSymTab,
         getGlobalScope,
         putGlobalScope,
         getFuncStates,
         putFuncStates,
         getFrameStack,
         putFrameStack,
         labelNum
        ) where


import qualified Data.Map    as M

import           GlobalScope (GlobalScope, mkGlobalScope)
import           LocalScope  (FuncState)
import           Stack       (Stack, mkStack)
import           SuccState   (SuccStateM, getState, putState)


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)


type GenState = SuccStateM SymTab


mkSymTab :: SymTab
mkSymTab = Tab 1 mkStack mkGlobalScope M.empty


getGlobalScope :: GenState GlobalScope
getGlobalScope = do
        state <- getState
        pure . globalScope $ state


putGlobalScope :: GlobalScope -> GenState ()
putGlobalScope gs = do
        state <- getState
        putState $ state { globalScope = gs }


getFuncStates :: GenState (M.Map String FuncState)
getFuncStates = do
        state <- getState
        pure . funcStates $ state


putFuncStates :: M.Map String FuncState -> GenState ()
putFuncStates fs = do
        state <- getState
        putState $ state { funcStates = fs }


getFrameStack :: GenState (Stack String)
getFrameStack = do
        state <- getState
        pure . frameStack $ state


putFrameStack :: Stack String -> GenState ()
putFrameStack stack = do
        state <- getState
        putState $ state { frameStack = stack }


labelNum :: GenState Int
labelNum = do
        l <- getLabel
        putLabel . succ $ l
        pure l


getLabel :: GenState Int
getLabel = do
        state <- getState
        pure . label $ state


putLabel :: Int -> GenState ()
putLabel n = do
        state <- getState
        putState $ state { label = n }
