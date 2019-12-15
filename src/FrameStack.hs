
module FrameStack
        (currentFunction,
         getScope,
         popFunctionName,
         pushFunctionName)
        where


import Types              (Stack(Stack))
import GenState           (GenState)
import GenTokens          (Scope(..))
import qualified GenState (getFrameStack, putFrameStack)


getScope :: GenState Scope
getScope = do
        curr <- currentFunction
        if curr == "global"
           then pure Global
           else pure Local


currentFunction :: GenState String
currentFunction = do
        currFuncName <- queryStack
        case currFuncName of
             Nothing   -> return "global"
             Just name -> return name


popFunctionName :: GenState ()
popFunctionName = do
        stack <- GenState.getFrameStack
        GenState.putFrameStack $ stackPop stack


pushFunctionName :: String -> GenState ()
pushFunctionName name = do
        stack <- GenState.getFrameStack
        GenState.putFrameStack $ stackPush name stack


{- Internal -}

queryStack :: GenState (Maybe String)
queryStack = stackPeek <$> GenState.getFrameStack


stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s)  = Just $ head s


stackPop :: Stack a -> Stack a
stackPop (Stack []) = Stack []
stackPop (Stack s)  = Stack $ tail s


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
