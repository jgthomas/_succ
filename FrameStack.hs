
module FrameStack (currentFunction,
                   currentScope,
                   newStack,
                   popFunctionName,
                   pushFunctionName) where


import Evaluator (Evaluator(Ev))
import NewTypes (SymTab(frameStack), Stack(Stack))


{- API -}

newStack :: Stack a
newStack = Stack []


currentScope:: Evaluator String
currentScope = currentFunction


currentFunction :: Evaluator String
currentFunction = do
        currFuncName <- queryStack
        case currFuncName of
             Nothing   -> return "global"
             Just name -> return name


popFunctionName :: Evaluator ()
popFunctionName = Ev $ \symTab ->
        let stack   = frameStack symTab
            symTab' = symTab { frameStack = stackPop stack }
            in
        ((), symTab')


pushFunctionName :: String -> Evaluator ()
pushFunctionName funcName = Ev $ \symTab ->
        let stack   = frameStack symTab
            symTab' = symTab { frameStack = stackPush funcName stack }
            in
        ((), symTab')


{- Internal -}

queryStack :: Evaluator (Maybe String)
queryStack = Ev $ \symTab -> (stackPeek $ frameStack symTab, symTab)


stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s) = Just $ head s


stackPop :: Stack a -> Stack a
stackPop (Stack []) = (Stack [])
stackPop (Stack s) = Stack (tail s)


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
