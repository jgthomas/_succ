
module SymTabLocalScope
        (initScope,
         closeScope,
         scopeDepth
        ) where


import qualified Data.Map          as M

import qualified FrameStack        (currentFunc)
import           GenState          (GenState)
import           SymbolTable       (FuncState (currentScope, scopes))
import           SymTabLocalShared (getFuncState, setFuncState)


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
