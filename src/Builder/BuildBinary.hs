
module Builder.BuildBinary (binary) where


import Builder.Directive   (emitLabel)
import Builder.Instruction (Jump (..), Set (..), add, andBits, comp, emitJump,
                            idivq, imul, literal, move, orBits, pop, push,
                            setBitIf, shiftBitsLeft, shiftBitsRight, sub,
                            xorBits)
import Builder.Register    (Register (..), reg, regModResult, scratch)
import Types.Operator      (BinaryOp (..), ShiftOp (..))


-- | Output asm for binary operators
binary :: String -> String -> BinaryOp -> Int -> Int -> String
binary expr1 expr2 binOp lab1 lab2 =
        case binOp of
             Plus               -> computeAdd expr1 expr2
             Minus              -> computeSub expr1 expr2
             Multiply           -> computeMul expr1 expr2
             Divide             -> computeDiv expr1 expr2
             Modulo             -> computeMod expr1 expr2
             Equal              -> comparison expr1 expr2 ++ setBitIf Equ
             NotEqual           -> comparison expr1 expr2 ++ setBitIf NEqu
             GreaterThan        -> comparison expr1 expr2 ++ setBitIf GThan
             LessThan           -> comparison expr1 expr2 ++ setBitIf LThan
             GThanOrEqu         -> comparison expr1 expr2 ++ setBitIf GThanE
             LThanOrEqu         -> comparison expr1 expr2 ++ setBitIf LThanE
             LogicalOR          -> logicalOR expr1 expr2 lab1 lab2
             LogicalAND         -> logicalAND expr1 expr2 lab1 lab2
             BitwiseXOR         -> computeBitwise expr1 expr2 xorBits
             BitwiseAND         -> computeBitwise expr1 expr2 andBits
             BitwiseOR          -> computeBitwise expr1 expr2 orBits
             ShiftOp LeftShift  -> computeShift expr1 expr2 shiftBitsLeft
             ShiftOp RightShift -> computeShift expr1 expr2 shiftBitsRight


computeShift :: String
             -> String
             -> (String -> String -> String)
             -> String
computeShift expr1 n f = expr1 ++ f n (reg RAX)


computeBitwise :: String
               -> String
               -> (String -> String -> String)
               -> String
computeBitwise expr1 expr2 f =
        loadValues expr1 expr2
        ++ f scratch (reg RAX)


logicalOR :: String -> String -> Int -> Int-> String
logicalOR expr1 expr2 nextLabel endLabel =
        expr1
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE nextLabel
        ++ move (literal 1) (reg RAX)
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ expr2
        ++ comp (literal 0) (reg RAX)
        ++ move (literal 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND expr1 expr2 nextLabel endLabel =
        expr1
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JNE nextLabel
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ expr2
        ++ comp (literal 0) (reg RAX)
        ++ move (literal 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


computeAdd :: String -> String -> String
computeAdd expr1 expr2 =
        loadValues expr1 expr2
        ++ add scratch (reg RAX)


computeMod :: String -> String -> String
computeMod expr1 expr2 =
        push (reg RDX)
        ++ loadValues expr2 expr1
        ++ idivq scratch
        ++ move regModResult (reg RAX)
        ++ pop (reg RDX)


computeDiv :: String -> String -> String
computeDiv expr1 expr2 =
        push (reg RDX)
        ++ loadValues expr2 expr1
        ++ idivq scratch
        ++ pop (reg RDX)


computeMul :: String -> String -> String
computeMul expr1 expr2 =
        loadValues expr1 expr2
        ++ imul scratch (reg RAX)


computeSub :: String -> String -> String
computeSub expr1 expr2 =
        loadValues expr2 expr1
        ++ sub scratch (reg RAX)


loadValues :: String -> String -> String
loadValues expr1 expr2 =
        expr1
        ++ push (reg RAX)
        ++ expr2
        ++ pop scratch


comparison :: String -> String -> String
comparison expr1 expr2 =
        loadValues expr1 expr2
        ++ comp (reg RAX) scratch
        ++ move (literal 0) (reg RAX)
