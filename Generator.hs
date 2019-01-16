
module Generator (genASM) where


import Lexer (Operator(..))
import Parser (Tree(..))
import SymTab


genASM :: Tree -> Evaluator String

genASM (ProgramNode functionList) = do
        prog <- mapM genASM functionList
        -- concat $ map genASM functionList
        return $ concat prog

genASM (FunctionNode name statementList) = do
        func <- mapM genASM statementList
        return $ functionName name ++ concat func
        --functionName name ++ (concat $ map genASM statementList)

genASM (DeclarationNode varName value) = do
        off <- addSymbol varName
        case value of
             Nothing     -> return $ loadValue 0 ++ varOnStack off
             Just value  -> genASM value

genASM (AssignmentNode varName value operator) = do
        off <- lookUp varName
        assign <- genASM value
        return $ assign ++ varOnStack off

genASM (VarNode varName) = do
        off <- lookUp varName
        return $ varOffStack off

genASM (ReturnNode tree) = do
        rtn <- genASM tree
        return $ rtn ++ returnStatement

genASM (ConstantNode n) = return $ loadValue n

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        return $ unode ++ (unary op)

genASM (BinaryNode left right op) = do
        lft <- genASM left
        rgt <- genASM right
        return $ binary lft rgt op


functionName :: String -> String
functionName f = ".globl "
                 ++ f
                 ++ "\n"
                 ++ f
                 ++ ":\n"
                 ++ "pushq %rbp\n"
                 ++ "movq %rsp, %rbp\n"


loadValue :: Int -> String
loadValue n = "movq $" ++ (show n) ++ ", %rax\n"


varOnStack :: Int -> String
varOnStack n = "movq %rax, " ++ (show n) ++ "(%rbp)\n"


varOffStack :: Int -> String
varOffStack n = "movq " ++ (show n) ++ "(%rbp), %rax\n"


returnStatement :: String
returnStatement = "movq %rbp, %rsp\n"
                  ++ "popq %rbp\n"
                  ++ "ret\n"


unary :: Operator -> String
unary o
   | o == Minus         = "neg %rax\n"
   | o == BitwiseCompl  = "not %rax\n"
   | o == LogicNegation = "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"


binary :: String -> String -> Operator -> String
binary loadVal1 loadVal2 o
   | o == Plus               = loadTwoValues loadVal1 loadVal2 ++ "addq %rcx, %rax\n"
   | o == Multiply           = loadTwoValues loadVal1 loadVal2 ++ "imul %rcx, %rax\n"
   | o == Minus              = loadTwoValues loadVal2 loadVal1 ++ "subq %rcx, %rax\n"
   | o == Divide             = loadTwoValues loadVal2 loadVal1 ++ "cqto\n" ++ "idivq %rcx\n"
   | o == Equal              = comparison loadVal1 loadVal2 ++ "sete %al\n"
   | o == NotEqual           = comparison loadVal1 loadVal2 ++ "setne %al\n"
   | o == GreaterThan        = comparison loadVal1 loadVal2 ++ "setg %al\n"
   | o == LessThan           = comparison loadVal1 loadVal2 ++ "setl %al\n"
   | o == GreaterThanOrEqual = comparison loadVal1 loadVal2 ++ "setge %al\n"
   | o == LessThanOrEqual    = comparison loadVal1 loadVal2 ++ "setle %al\n"
   | o == LogicalOR          = loadTwoValues loadVal1 loadVal2
                               ++ "orq %rcx, %rax\n"
                               ++ "movq $0, %rax\n"
                               ++ "setne %al\n"
   | o == LogicalAND         = loadTwoValues loadVal1 loadVal2
                               ++ "cmpq $0, %rcx\n"
                               ++ "setne %cl\n"
                               ++ "cmpq $0, %rax\n"
                               ++ "movq $0, %rax\n"
                               ++ "setne %al\n"
                               ++ "andb %cl, %al\n"


loadTwoValues :: String -> String -> String
loadTwoValues loadVal1 loadVal2 = loadVal1
                                  ++ "pushq %rax\n"
                                  ++ loadVal2
                                  ++ "popq %rcx\n"


comparison :: String -> String -> String
comparison loadVal1 loadVal2 = loadTwoValues loadVal1 loadVal2
                               ++ "cmpq %rax, %rcx\n"
                               ++ "movq $0, %rax\n"
