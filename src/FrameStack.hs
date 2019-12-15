{-|
Module       : FrameStack
Description  : Tracks current function

Keeps track of the function currently being compiled.
-}
module FrameStack
        (currentFunction,
         getScope,
         popFunctionName,
         pushFunctionName)
        where


import GenState  (GenState)
import GenTokens (Scope (..))
import Stack     (Stack, stackPeek, stackPop, stackPush)
import SuccState (getState, putState)
import Types     (frameStack)


-- | Check if in Local or Global scope
getScope :: GenState Scope
getScope = do
        curr <- currentFunction
        if curr == "global"
           then pure Global
           else pure Local


-- | Return name of the current function being compiled
currentFunction :: GenState String
currentFunction = do
        currFuncName <- stackPeek <$> getFrameStack
        case currFuncName of
             Nothing   -> pure "global"
             Just name -> pure name


-- | Remove function name from top of stack
popFunctionName :: GenState ()
popFunctionName = do
        stack <- getFrameStack
        putFrameStack $ stackPop stack


-- | Add function name to top of stack
pushFunctionName :: String -> GenState ()
pushFunctionName name = do
        stack <- getFrameStack
        putFrameStack $ stackPush name stack


getFrameStack :: GenState (Stack String)
getFrameStack = do
        state <- getState
        return . frameStack $ state


putFrameStack :: Stack String -> GenState ()
putFrameStack stack = do
        state <- getState
        putState $ state { frameStack = stack }
