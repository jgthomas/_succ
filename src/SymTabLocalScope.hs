
module SymTabLocalScope where


import qualified Data.Map          as M

import           Error             (CompilerError (StateError), StateError (..))
import qualified FrameStack        (currentFunc)
import           GenState          (GenState, throwError)
import           GenStateLocal     (FuncState (currentScope, scopes), LocalVar)
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


getScope :: Int -> FuncState -> GenState (M.Map String LocalVar)
getScope scope fs =
        case M.lookup scope $ scopes fs of
             Just sc -> pure sc
             Nothing -> do
                     funcName <- FrameStack.currentFunc
                     throwError $ StateError (UndefinedScope funcName scope)


addNestedScope :: String -> Int -> GenState ()
addNestedScope name level = do
        fs <- getFuncState name
        let fs' = fs { scopes = M.insert level M.empty $ scopes fs }
        setFuncState name fs'


incrementScope :: GenState Int
incrementScope = stepScope succ


decrementScope :: GenState Int
decrementScope = stepScope pred


findScope :: String -> GenState Int
findScope name = currentScope <$> getFuncState name


stepScope :: (Int -> Int) -> GenState Int
stepScope f = do
        funcName  <- FrameStack.currentFunc
        funcState <- getFuncState funcName
        let newLevel = f . currentScope $ funcState
        setFuncState funcName funcState { currentScope = newLevel }
        pure newLevel
