
module SimpleStack (newStack,
                    currentFunction,
                    inGlobalScope,
                    popFunctionName,
                    pushFunctionName) where


import Types (SymTab(nameStack), Stack(Stack))
import Evaluator (Evaluator(Ev))


currentFunction :: Evaluator String
currentFunction = do
        currFuncName <- queryStack
        case currFuncName of
             Nothing   -> return "global"
             Just name -> return name


inGlobalScope :: Evaluator Bool
inGlobalScope = do
        global <- queryStack
        case global of
             Nothing -> return True
             Just f  -> return False


popFunctionName :: Evaluator Bool
popFunctionName = Ev $ \symTab ->
        let stack = nameStack symTab
            symTab' = symTab { nameStack = stackPop stack }
            in
        (True, symTab')


pushFunctionName :: String -> Evaluator Bool
pushFunctionName funcName = Ev $ \symTab ->
        let stack = nameStack symTab
            symTab' = symTab { nameStack = stackPush funcName stack }
            in
        (True, symTab')


queryStack :: Evaluator (Maybe String)
queryStack = Ev $ \symTab -> (stackPeek $ nameStack symTab, symTab)


newStack :: Stack a
newStack = Stack []


stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s) = Just $ head s


stackPop :: Stack a -> Stack a
stackPop (Stack []) = (Stack [])
stackPop (Stack s) = Stack (tail s)


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
