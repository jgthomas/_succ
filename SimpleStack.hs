
module SimpleStack (newStack,
                    stackPeek,
                    stackPop,
                    stackPush) where


import Types (Stack(..))

--newtype Stack a = Stack [a] deriving Show


newStack :: Stack a
newStack = Stack []


stackPeek :: Stack a -> a
stackPeek (Stack s) = head s


stackPop :: Stack a -> Stack a
stackPop (Stack s) = Stack (tail s)


stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)
