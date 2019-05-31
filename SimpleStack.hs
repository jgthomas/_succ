
module SimpleStack (newStack,
                    currentFunction,
                    popFunctionName,
                    pushFunctionName) where


import Types (SymTab(nameStack), Stack(Stack))
import Evaluator (Evaluator(Ev))


currentFunction :: Evaluator String
currentFunction = Ev $ \symTab ->
        let currFuncName = stackPeek $ nameStack symTab
            in
        case currFuncName of
             Nothing   -> ("global", symTab)
             Just name -> (name, symTab)


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
