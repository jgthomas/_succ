
module AsmUnary (unary) where


import AsmVariables (saveGlobal, varOnStack)
import GenTokens    (VarType (..))
import Instruction  (Set (..), comp, dec, inc, invertBits, literal,
                     makeNegative, move, setBitIf)
import Operator     (PostOpUnary (..), PreOpUnary (..), Unary (..),
                     UnaryOp (..))
import Register     (Register (..), reg, scratch)


-- | Output asm for unary operators
unary :: String -> UnaryOp -> VarType -> String
unary load (PreOpUnary op) var  = load ++ unaryPreOp op var
unary load (PostOpUnary op) var = load ++ unaryPostOp op var
unary load (Unary op) _         = load ++ unaryOp op


unaryPreOp :: PreOpUnary -> VarType -> String
unaryPreOp PreIncrement (LocalVar n m)  = inc (reg RAX) ++ varOnStack (n + m)
unaryPreOp PreDecrement (LocalVar n m)  = dec (reg RAX) ++ varOnStack (n + m)
unaryPreOp _ ParamVar{}                 = undefined
unaryPreOp PreIncrement (GlobalVar s _) = inc (reg RAX) ++ saveGlobal s
unaryPreOp PreDecrement (GlobalVar s _) = dec (reg RAX) ++ saveGlobal s


unaryPostOp :: PostOpUnary -> VarType -> String
unaryPostOp PostIncrement (LocalVar n m)  = updateStoredLocal (n + m) inc
unaryPostOp PostDecrement (LocalVar n m)  = updateStoredLocal (n + m) dec
unaryPostOp _ ParamVar{}                  = undefined
unaryPostOp PostIncrement (GlobalVar s _) = updateStoredGlobal s inc
unaryPostOp PostDecrement (GlobalVar s _) = updateStoredGlobal s dec


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
             Positive    -> ""
             BitwiseComp -> invertBits (reg RAX)
             LogicalNeg  -> logNeg


logNeg :: String
logNeg = comp (literal 0) (reg RAX)
         ++ move (literal 0) (reg RAX)
         ++ setBitIf Equ
