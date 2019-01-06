
module Generator (extractFrom, progString) where


import Parser


extractFrom :: Tree -> [String]

extractFrom (ProgramNode tree) = extractFrom tree

extractFrom (FunctionNode name tree) = [name] ++ extractFrom tree

extractFrom (StatementNode kwd tree) = extractFrom tree

extractFrom (ExpressionNode n) = [(show n)]


progString :: [String] -> String
progString (f:e:str) = ".globl " ++ f ++ "\n" ++ f ++ ":\n" ++ "movq $" ++ e ++ ", %rax\nret\n"
