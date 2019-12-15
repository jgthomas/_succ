{-|
Module       : Stack
Description  : Simple stack implementation
-}
module Stack
        (Stack,
         mkStack,
         stackPush,
         stackPop,
         stackPeek
        ) where


-- | Stack definition
newtype Stack a = Stack [a] deriving Show


-- | Stack constructor
mkStack :: Stack a
mkStack = Stack []


-- | Push element onto stack
stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)


-- | Pop element off stack
stackPop :: Stack a -> Stack a
stackPop (Stack []) = Stack []
stackPop (Stack s)  = Stack $ tail s


-- | Look at top element
stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s)  = Just $ head s
