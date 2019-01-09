
module Generator (genASM) where


import Lexer (Operator(..))
import Parser (Tree(..))


genASM :: Tree -> String

genASM (ProgramNode tree) = genASM tree

genASM (FunctionNode name tree) = functionName name ++ genASM tree

genASM (ReturnNode tree) = genASM tree ++ returnStatement

genASM (ConstantNode n) = loadValue n

genASM (UnaryNode tree op) = genASM tree ++ unary op

genASM (BinaryNode left right op) = binary (genASM left) (genASM right) op


functionName :: String -> String
functionName f = ".globl " ++ f ++ "\n" ++ f ++ ":\n"

loadValue :: Int -> String
loadValue n = "movq $" ++ (show n) ++ ", %rax\n"

returnStatement :: String
returnStatement = "ret\n"

unary :: Operator -> String
unary o
   | o == Minus         = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"

binary :: String -> String -> Operator -> String
binary val1 val2 o
   | o == Plus       = val1 ++ "pushq %rax\n" ++ val2 ++ "popq %rcx\n" ++ "addq %rcx, %rax\n"
   | o == Multiply   = val1 ++ "pushq %rax\n" ++ val2 ++ "popq %rcx\n" ++ "imul %rcx, %rax\n"
   | o == Minus      = val2 ++ "pushq %rax\n" ++ val1 ++ "popq %rcx\n" ++ "subq %rcx, %rax\n"
   | o == Divide     = val1 ++ "pushq %rax\n" ++ val2 ++ "movq %rax, %rbx\n" ++ "popq %rax\n" ++ "cqto\n" ++ "idivq %rbx\n"
