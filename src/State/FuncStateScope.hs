{-|
Module       : FuncStateScope
Description  : Manages nested scopes

Functions for creating and managing nested scopes within functions under
compilation.
-}
module State.FuncStateScope
        (initFunction,
         closeFunction,
         initScope,
         closeScope,
         scopeDepth
        ) where


import           Control.Monad         (when)
import qualified Data.Map              as M
import           Data.Maybe            (isNothing)

import qualified State.FrameStack      as FrameStack (currentFunc, popFunc,
                                                      pushFunc)
import           State.FuncStateAccess (getFuncState, grabFuncState,
                                        setFuncState)
import           State.GenState        (GenState)
import           State.SymbolTable     (FuncState (currentScope, scopes))
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
