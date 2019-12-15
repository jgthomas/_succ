{-|
Module       : FrameStack
Description  : Tracks current function

Keeps track of the function currently being compiled.
-}
module FrameStack
        (currentFunc,
         getScope,
         popFunc,
         pushFunc)
        where


import           GenState  (GenState)
import qualified GenState  (getFrameStack, putFrameStack)
import           GenTokens (Scope (..))
import           Stack     (stackPeek, stackPop, stackPush)


-- | Check if in Local or Global scope
getScope :: GenState Scope
getScope = do
        curr <- currentFunc
        if curr == "global"
           then pure Global
           else pure Local


-- | Return name of the current function being compiled
currentFunc :: GenState String
currentFunc = do
        currFuncName <- stackPeek <$> GenState.getFrameStack
        case currFuncName of
             Nothing   -> pure "global"
             Just name -> pure name


-- | Remove function name from top of stack
popFunc :: GenState ()
popFunc = do
        stack <- GenState.getFrameStack
        GenState.putFrameStack $ stackPop stack


-- | Add function name to top of stack
pushFunc :: String -> GenState ()
pushFunc name = do
        stack <- GenState.getFrameStack
        GenState.putFrameStack $ stackPush name stack
