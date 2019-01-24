
module Generator (genASM) where


import Lexer (Operator(..))
import Parser (Tree(..))
import SymTab


genASM :: Tree -> Evaluator String

genASM (ProgramNode functionList) = do
        prog <- mapM genASM functionList
        return $ concat prog

genASM (FunctionNode name statementList) = do
        if length statementList == 0
           then return $ functionName name ++ loadValue 0 ++ returnStatement
           else do
                   funcStmnts <- mapM genASM statementList
                   return $ functionName name ++ concat funcStmnts

genASM (CompoundStmtNode blockItems) = do
        blockLines <- mapM genASM blockItems
        return $ concat blockLines

genASM (IfNode test action possElse) = do
        testVal <- genASM test
        ifAction <- genASM action
        label <- labelNum
        let ifLines = testVal
                      ++ "cmpq $0, %rax\n"
                      ++ "je _label_" ++ (show label) ++ "\n"
                      ++ ifAction
        case possElse of
             Nothing       -> return $ ifLines
                                       ++ "_label_" ++ (show label) ++ ":\n"
             Just possElse -> do
                     elseAction <- genASM possElse
                     nextLabel <- labelNum
                     return $ ifLines
                              ++ "jmp _label_" ++ (show nextLabel) ++ "\n"
                              ++ "_label_" ++ (show label) ++ ":\n"
                              ++ elseAction
                              ++ "_label_" ++ (show nextLabel) ++ ":\n"

genASM (DeclarationNode varName value) = do
        varDeclared <- checkVar varName
        case varDeclared of
             True  -> error $ "Variable '" ++ varName ++ "' already declared"
             False -> do
                     offset <- addSymbol varName
                     case value of
                          Nothing     -> return $ loadValue 0 ++ varOnStack offset
                          Just value  -> genASM value

genASM (AssignmentNode varName value operator) = do
        offset <- lookUp varName
        assign <- genASM value
        return $ assign ++ varOnStack offset

genASM (ExprStmtNode expression) = do
        exprsn <- genASM expression
        return exprsn

genASM (ReturnNode tree) = do
        rtn <- genASM tree
        return $ rtn ++ returnStatement

genASM (TernaryNode cond pass fail) = do
        testVal <- genASM cond
        passAction <- genASM pass
        failAction <- genASM fail
        failLabel <- labelNum
        passLabel <- labelNum
        return $ testVal
                 ++ "cmpq $0, %rax\n"
                 ++ "je _label_" ++ (show failLabel) ++ "\n"
                 ++ passAction
                 ++ "jmp _label_" ++ (show passLabel) ++ "\n"
                 ++ "_label_" ++ (show failLabel) ++ ":\n"
                 ++ failAction
                 ++ "_label_" ++ (show passLabel) ++ ":\n"

genASM (BinaryNode left right op) = do
        lft <- genASM left
        rgt <- genASM right
        return $ binary lft rgt op

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        return $ unode ++ (unary op)

genASM (VarNode varName) = do
        offset <- lookUp varName
        return $ varOffStack offset

genASM (ConstantNode n) = return $ loadValue n


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
               ++ "subq $8, %rsp\n"


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
