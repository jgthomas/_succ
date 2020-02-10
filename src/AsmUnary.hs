

module AsmUnary (unary) where


import AsmShared    (empty, literalValue)
import AsmVariables (saveGlobal, varOnStack)
import GenState     (GenState)
import Instruction
import Operator     (PostOpUnary (..), PreOpUnary (..), Unary (..),
                     UnaryOp (..))
import Register


-- | Output asm for unary operators
unary :: String
      -> UnaryOp
      -> Maybe Int
      -> Maybe String
      -> GenState String
unary load (PreOpUnary op) n l  = pure $ load ++ unaryPreOp op n l
unary load (PostOpUnary op) n l = pure $ load ++ unaryPostOp op n l
unary load (Unary op) _ _       = pure $ load ++ unaryOp op


unaryPreOp :: PreOpUnary -> Maybe Int -> Maybe String -> String
unaryPreOp PreIncrement (Just n) _ = inc (reg RAX) ++ varOnStack n
unaryPreOp PreDecrement (Just n) _ = dec (reg RAX) ++ varOnStack n
unaryPreOp PreIncrement _ (Just l) = inc (reg RAX) ++ saveGlobal l
unaryPreOp PreDecrement _ (Just l) = dec (reg RAX) ++ saveGlobal l
unaryPreOp _ _ _                   = undefined


unaryPostOp :: PostOpUnary -> Maybe Int -> Maybe String -> String
unaryPostOp PostIncrement (Just n) _ = updateStoredLocal n inc
unaryPostOp PostDecrement (Just n) _ = updateStoredLocal n dec
unaryPostOp PostIncrement _ (Just l) = updateStoredGlobal l inc
unaryPostOp PostDecrement _ (Just l) = updateStoredGlobal l dec
unaryPostOp _ _ _                    = undefined


updateStoredLocal :: Int -> (String -> String) -> String
updateStoredLocal n f =
        move (reg RAX) scratch
        ++ f (reg RAX)
        ++ varOnStack n
        ++ move scratch (reg RAX)


updateStoredGlobal :: String -> (String -> String) -> String
updateStoredGlobal l f =
        move (reg RAX) scratch
        ++ f (reg RAX)
        ++ saveGlobal l
        ++ move scratch (reg RAX)


unaryOp :: Unary -> String
unaryOp unOp =
        case unOp of
             Negate      -> makeNegative (reg RAX)
             Positive    -> empty
             BitwiseComp -> invertBits (reg RAX)
             LogicalNeg  -> logNeg


logNeg :: String
logNeg = comp (literalValue 0) (reg RAX)
         ++ move (literalValue 0) (reg RAX)
         ++ setBitIf Equ


