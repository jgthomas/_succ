
module Generator (genASM) where


import Lexer (Operator(..))
import Parser (Tree(..))


genASM :: Tree -> String

genASM (ProgramNode tree) = genASM tree

genASM (FunctionNode name tree) = functionName name ++ genASM tree

genASM (ReturnNode tree) = genASM tree ++ returnStatement

genASM (ConstantNode n) = pushValue n

genASM (UnaryNode tree op) = genASM tree ++ unary op

genASM (BinaryNode left right op) = binary (genASM left) (genASM right) op


functionName :: String -> String
functionName f = ".globl " ++ f ++ "\n" ++ f ++ ":\n"

pushValue :: Int -> String
pushValue n = "movq $" ++ (show n) ++ ", %rax\n"

popResult :: String
popResult = "popq %rax\n"

storeResult :: String
storeResult = "pushq %rax\n"

returnStatement :: String
returnStatement = "ret\n"

unary :: Operator -> String
unary o
   | o == Minus         = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"

binary :: String -> String -> Operator -> String
binary i1 i2 o
   | o == Plus          = i1 ++ "pushq %rax\n" ++ i2 ++ "popq %rcx\n" ++ "addq %rcx, %rax\n"
   | o == Multiply      = i1 ++ "pushq %rax\n" ++ i2 ++ "popq %rcx\n" ++ "imul %rcx, %rax\n"
   | o == Minus         = i2 ++ "pushq %rax\n" ++ i1 ++ "popq %rcx\n" ++ "subq %rcx, %rax\n"
   | o == Divide        = i2 ++ "movq %rax, %rbx\n" ++ i1 ++ "cqto\n" ++ "idivq %rbx\n"
