
module FrameStack (currentFunction,
                   currentScope,
                   popFunctionName,
                   pushFunctionName) where


import Types (SymTab(frameStack), Stack(Stack))
import SuccState (GenState, getState, putState)


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
        state <- getState
        putState $ state { frameStack = stackPop $ frameStack state }


pushFunctionName :: String -> GenState ()
pushFunctionName name = do
        state <- getState
        putState $ state { frameStack = stackPush name $ frameStack state }


{- Internal -}

queryStack :: GenState (Maybe String)
queryStack = do
        state <- getState
        return $ stackPeek . frameStack $ state


stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s)  = Just $ head s


stackPop :: Stack a -> Stack a
stackPop (Stack []) = Stack []
stackPop (Stack s)  = Stack $ tail s


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
