
module Generator (genASM) where


import Lexer
import Parser


genASM :: Tree -> String

genASM (ProgramNode tree) = genASM tree

genASM (FunctionNode name tree) = functionName name ++ genASM tree

genASM (ReturnNode tree) = genASM tree ++ returnStatement

genASM (ConstantNode n) = returnValue (show n)

genASM (UnaryNode tree unop) = genASM tree ++ unary unop


functionName :: String -> String
functionName f = ".globl " ++ f ++ "\n" ++ f ++ ":\n"

returnValue :: String -> String
returnValue n = "movq $" ++ n ++ ", %rax\n"

returnStatement :: String
returnStatement = "ret\n"

unary :: UnaryOperator -> String
unary o
   | o == Negation      = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"
