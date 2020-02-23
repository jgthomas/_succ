
module AsmUnary (unary) where


import AsmVariables (storeVariable)
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
unaryPreOp PreIncrement var@LocalVar{}  = inc (reg RAX) ++ storeVariable var--varOnStack (n + m)
unaryPreOp PreDecrement var@LocalVar{}  = dec (reg RAX) ++ storeVariable var--varOnStack (n + m)
unaryPreOp _ ParamVar{}                 = undefined
unaryPreOp PreIncrement var@GlobalVar{} = inc (reg RAX) ++ storeVariable var--saveGlobal s
unaryPreOp PreDecrement var@GlobalVar{} = dec (reg RAX) ++ storeVariable var--saveGlobal s


unaryPostOp :: PostOpUnary -> VarType -> String
unaryPostOp PostIncrement var@LocalVar{}  = updateStoredLocal var inc
unaryPostOp PostDecrement var@LocalVar{}  = updateStoredLocal var dec
unaryPostOp _ ParamVar{}                  = undefined
unaryPostOp PostIncrement var@GlobalVar{} = updateStoredGlobal var inc
unaryPostOp PostDecrement var@GlobalVar{} = updateStoredGlobal var dec


updateStoredLocal :: VarType -> (String -> String) -> String
updateStoredLocal var f =
        move (reg RAX) scratch
        ++ f (reg RAX)
        ++ storeVariable var
        ++ move scratch (reg RAX)


updateStoredGlobal :: VarType -> (String -> String) -> String
updateStoredGlobal var f =
        move (reg RAX) scratch
        ++ f (reg RAX)
        ++ storeVariable var
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
