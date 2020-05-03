

module Asm.AsmBinary (binary) where


import Asm.Directive   (emitLabel)
import Asm.Instruction (Jump (..), Set (..), add, andBits, comp, emitJump,
                        idivq, imul, literal, move, orBits, pop, push, setBitIf,
                        shiftBitsLeft, shiftBitsRight, sub, xorBits)
import Asm.Register    (Register (..), reg, regModResult, scratch)
import Types.Operator  (BinaryOp (..), ShiftOp (..))


-- | Output asm for binary operators
binary :: String -> String -> BinaryOp -> Int -> Int -> String
binary load1 load2 binOp lab1 lab2 =
        case binOp of
             Plus               -> computeAdd load1 load2
             Minus              -> computeSub load1 load2
             Multiply           -> computeMul load1 load2
             Divide             -> computeDiv load1 load2
             Modulo             -> computeMod load1 load2
             Equal              -> comparison load1 load2 ++ setBitIf Equ
             NotEqual           -> comparison load1 load2 ++ setBitIf NEqu
             GreaterThan        -> comparison load1 load2 ++ setBitIf GThan
             LessThan           -> comparison load1 load2 ++ setBitIf LThan
             GThanOrEqu         -> comparison load1 load2 ++ setBitIf GThanE
             LThanOrEqu         -> comparison load1 load2 ++ setBitIf LThanE
             LogicalOR          -> logicalOR load1 load2 lab1 lab2
             LogicalAND         -> logicalAND load1 load2 lab1 lab2
             BitwiseXOR         -> computeBitwise load1 load2 xorBits
             BitwiseAND         -> computeBitwise load1 load2 andBits
             BitwiseOR          -> computeBitwise load1 load2 orBits
             ShiftOp LeftShift  -> computeShift load1 load2 shiftBitsLeft
             ShiftOp RightShift -> computeShift load1 load2 shiftBitsRight


computeShift :: String
             -> String
             -> (String -> String -> String)
             -> String
computeShift load1 n f = load1 ++ f n (reg RAX)


computeBitwise :: String
               -> String
               -> (String -> String -> String)
               -> String
computeBitwise load1 load2 f =
        loadValues load1 load2
        ++ f scratch (reg RAX)


logicalOR :: String -> String -> Int -> Int -> String
logicalOR load1 load2 nextLabel endLabel =
        load1
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JE nextLabel
        ++ move (literal 1) (reg RAX)
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ comp (literal 0) (reg RAX)
        ++ move (literal 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND load1 load2 nextLabel endLabel =
        load1
        ++ comp (literal 0) (reg RAX)
        ++ emitJump JNE nextLabel
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ comp (literal 0) (reg RAX)
        ++ move (literal 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


computeAdd :: String -> String -> String
computeAdd load1 load2 =
        loadValues load1 load2
        ++ add scratch (reg RAX)


computeMod :: String -> String -> String
computeMod load1 load2 =
        push (reg RDX)
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ move regModResult (reg RAX)
        ++ pop (reg RDX)


computeDiv :: String -> String -> String
computeDiv load1 load2 =
        push (reg RDX)
        ++ loadValues load2 load1
        ++ idivq scratch
        ++ pop (reg RDX)


computeMul :: String -> String -> String
computeMul load1 load2 =
        loadValues load1 load2
        ++ imul scratch (reg RAX)


computeSub :: String -> String -> String
computeSub load1 load2 =
        loadValues load2 load1
        ++ sub scratch (reg RAX)


loadValues :: String -> String -> String
loadValues load1 load2 =
        load1
        ++ push (reg RAX)
        ++ load2
        ++ pop scratch


comparison :: String -> String -> String
comparison load1 load2 =
        loadValues load1 load2
        ++ comp (reg RAX) scratch
        ++ move (literal 0) (reg RAX)
