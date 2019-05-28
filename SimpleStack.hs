
module SimpleStack (newStack,
                    currentFunction,
                    popFunctionName,
                    pushFunctionName) where


import Types (SymTab(..), Stack(..))
import Evaluator (Evaluator(Ev))


currentFunction :: Evaluator String
currentFunction = Ev $ \symTab ->
        let stack = nameStack symTab
            currFuncName = stackPeek stack
            in
        (currFuncName, symTab)


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


stackPeek :: Stack a -> a
stackPeek (Stack s) = head s


stackPop :: Stack a -> Stack a
stackPop (Stack s) = Stack (tail s)


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
