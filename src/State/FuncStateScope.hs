{-|
Module       : FuncStateScope
Description  : Manages nested scopes

Functions for creating and managing nested scopes within functions under
compilation.
-}
module State.FuncStateScope
        (initScope,
         closeScope,
         scopeDepth
        ) where


import qualified Data.Map              as M

import qualified State.FrameStack      as FrameStack (currentFunc)
import           State.FuncStateAccess (getFuncState, setFuncState)
import           State.GenState        (GenState)
import           State.SymbolTable     (FuncState (currentScope, scopes))


-- | Initialize a new scope inside a function
initScope :: GenState ()
initScope = do
        currFuncName  <- FrameStack.currentFunc
        newScopeLevel <- incrementScope
        addNestedScope currFuncName newScopeLevel


-- | Exit current scope inside a function
closeScope :: GenState ()
closeScope = do
        _ <- decrementScope
        pure ()


-- | Return scope depth of current function
scopeDepth :: String -> GenState Int
scopeDepth name = currentScope <$> getFuncState name


addNestedScope :: String -> Int -> GenState ()
addNestedScope name level = do
        fs <- getFuncState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFuncState name fs'


incrementScope :: GenState Int
incrementScope = stepScope succ


decrementScope :: GenState Int
decrementScope = stepScope pred


stepScope :: (Int -> Int) -> GenState Int
stepScope f = do
        funcName  <- FrameStack.currentFunc
        funcState <- getFuncState funcName
        let newLevel = f . currentScope $ funcState
        setFuncState funcName funcState { currentScope = newLevel }
        pure newLevel
