

module AsmBinary (binary) where


import AsmShared   (literalValue, testResult)
import Assembly    (Jump (..), Set (..))
import Directive   (emitLabel)
import GenState    (GenState)
import Instruction
import Operator    (BinaryOp (..), ShiftOp (..))
import Register


-- | Output asm for binary operators
binary :: String -> String -> BinaryOp -> Int -> Int -> GenState String
binary load1 load2 binOp lab1 lab2 =
        case binOp of
             Plus               -> pure $ computeAdd load1 load2
             Minus              -> pure $ computeSub load1 load2
             Multiply           -> pure $ computeMul load1 load2
             Divide             -> pure $ computeDiv load1 load2
             Modulo             -> pure $ computeMod load1 load2
             Equal              -> pure $ comparison load1 load2 ++ setBitIf Equ
             NotEqual           -> pure $ comparison load1 load2 ++ setBitIf NEqu
             GreaterThan        -> pure $ comparison load1 load2 ++ setBitIf GThan
             LessThan           -> pure $ comparison load1 load2 ++ setBitIf LThan
             GThanOrEqu         -> pure $ comparison load1 load2 ++ setBitIf GThanE
             LThanOrEqu         -> pure $ comparison load1 load2 ++ setBitIf LThanE
             LogicalOR          -> pure $ logicalOR load1 load2 lab1 lab2
             LogicalAND         -> pure $ logicalAND load1 load2 lab1 lab2
             BitwiseXOR         -> pure $ computeBitwise load1 load2 xorBits
             BitwiseAND         -> pure $ computeBitwise load1 load2 andBits
             BitwiseOR          -> pure $ computeBitwise load1 load2 orBits
             ShiftOp LeftShift  -> pure $ computeShift load1 load2 shiftBitsLeft
             ShiftOp RightShift -> pure $ computeShift load1 load2 shiftBitsRight


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
        ++ testResult
        ++ emitJump JE nextLabel
        ++ move (literalValue 1) (reg RAX)
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move (literalValue 0) (reg RAX)
        ++ setBitIf NEqu
        ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND load1 load2 nextLabel endLabel =
        load1
        ++ testResult
        ++ emitJump JNE nextLabel
        ++ emitJump JMP endLabel
        ++ emitLabel nextLabel
        ++ load2
        ++ testResult
        ++ move (literalValue 0) (reg RAX)
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
        ++ move (literalValue 0) (reg RAX)
