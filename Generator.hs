
module Generator (extractFrom) where


import Lexer
import Parser


extractFrom :: Tree -> String

extractFrom (ProgramNode tree) = extractFrom tree

extractFrom (FunctionNode name tree) = functionName name ++ extractFrom tree

extractFrom (ReturnNode tree) = extractFrom tree ++ returnStatement

extractFrom (ConstantNode n) = returnValue (show n)

extractFrom (UnaryNode tree unop) = extractFrom tree ++ unary unop


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
