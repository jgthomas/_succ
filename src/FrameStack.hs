
module FrameStack
        (currentFunction,
         currentScope,
         popFunctionName,
         pushFunctionName)
        where


import Types                      (Stack(Stack))
import GenState                   (GenState)
import qualified GenState as GenS (getFrameStack, putFrameStack)


{- API -}

currentScope:: GenState String
currentScope = currentFunction


currentFunction :: GenState String
currentFunction = do
        currFuncName <- queryStack
        case currFuncName of
             Nothing   -> return "global"
             Just name -> return name


popFunctionName :: GenState ()
popFunctionName = do
        stack <- GenS.getFrameStack
        GenS.putFrameStack $ stackPop stack


pushFunctionName :: String -> GenState ()
pushFunctionName name = do
        stack <- GenS.getFrameStack
        GenS.putFrameStack $ stackPush name stack


{- Internal -}

queryStack :: GenState (Maybe String)
queryStack = stackPeek <$> GenS.getFrameStack


stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s)  = Just $ head s


stackPop :: Stack a -> Stack a
stackPop (Stack []) = Stack []
stackPop (Stack s)  = Stack $ tail s


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
