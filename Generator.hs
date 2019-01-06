
module Generator (generate, progString) where


import Parser


generate :: Tree -> [String]

generate (ProgramNode tree) = generate tree

generate (FunctionNode name tree) = [name] ++ generate tree

generate (StatementNode kwd tree) = generate tree

generate (ExpressionNode n) = [(show n)]


progString :: [String] -> String
progString (f:e:str) = ".globl " ++ f ++ "\n" ++ f ++ ":\n" ++ "movq $" ++ e ++ ", %rax\nret\n"
