
module Generator (extractFrom, progString) where


import Parser


extractFrom :: Tree -> [String]

extractFrom (ProgramNode tree) = extractFrom tree

extractFrom (FunctionNode name tree) = [name] ++ extractFrom tree

extractFrom (StatementNode kwd tree) = extractFrom tree

extractFrom (ConstantNode n) = [(show n)]

extractFrom (UnaryNode unop tree) = [(show unop)] ++ extractFrom tree


progString :: [String] -> String
progString (f:n:str) = (header f) ++ (returnVal n)


header :: String -> String
header f = ".globl " ++ f ++ "\n" ++ f ++ ":\n"


returnVal :: String -> String
returnVal n = "movq $" ++ n ++ ", %rax\nret\n"
