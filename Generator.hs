
module Generator (extractFrom, progString) where


import Parser


extractFrom :: Tree -> [String]

extractFrom (ProgramNode tree) = extractFrom tree

extractFrom (FunctionNode name tree) = [name] ++ extractFrom tree

extractFrom (ReturnNode tree) = extractFrom tree

extractFrom (ConstantNode n) = [(show n)]

extractFrom (UnaryNode tree unop) = extractFrom tree ++ [(show unop)]


progString :: [String] -> String
progString (f:n:str) = (header f) ++ (returnVal n)


header :: String -> String
header f = ".globl " ++ f ++ "\n" ++ f ++ ":\n"


returnVal :: String -> String
returnVal n = "movq $" ++ n ++ ", %rax\nret\n"
