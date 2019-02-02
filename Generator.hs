
module Generator (genASM) where


import Lexer (Operator(..))
import Parser (Tree(..))
import SymTab


data Jump = JMP
          | JE
          deriving Eq


genASM :: Tree -> Evaluator String

genASM (ProgramNode functionList) = do
        prog <- mapM genASM functionList
        return $ concat prog

genASM (FunctionNode name statementList) = do
        initScope
        funcStmnts <- mapM genASM statementList
        closeScope
        case hasReturn statementList of
             True  -> return $ functionName name
                               ++ concat funcStmnts
             False ->
                     if name == "main"
                        then do
                                -- return 0 if no return specified
                                return $ functionName name
                                         ++ concat funcStmnts
                                         ++ loadValue 0
                                         ++ returnStatement
                        else do
                                -- undefined if used by caller
                                return $ functionName name
                                         ++ concat funcStmnts

genASM (CompoundStmtNode blockItems) = do
        initScope
        blockLines <- mapM genASM blockItems
        closeScope
        return $ concat blockLines

genASM (WhileNode test whileBlock) = do
        loopLabel <- labelNum
        test <- genASM test
        testLabel <- labelNum
        body <- genASM whileBlock
        return $ (emitLabel loopLabel)
                 ++ test
                 ++ testResult
                 ++ (emitJump JE testLabel)
                 ++ body
                 ++ (emitJump JMP loopLabel)
                 ++ (emitLabel testLabel)

genASM (IfNode test action possElse) = do
        testVal <- genASM test
        ifAction <- genASM action
        label <- labelNum
        let ifLines = testVal
                      ++ testResult
                      ++ (emitJump JE label)
                      ++ ifAction
        case possElse of
             Nothing       -> return $ ifLines ++ (emitLabel label)
             Just possElse -> do
                     elseAction <- genASM possElse
                     nextLabel <- labelNum
                     return $ ifLines
                              ++ (emitJump JMP nextLabel)
                              ++ (emitLabel label)
                              ++ elseAction
                              ++ (emitLabel nextLabel)

genASM (DeclarationNode varName value) = do
        varDeclared <- checkVar varName
        case varDeclared of
             True  -> error $ "Variable '" ++ varName ++ "' already declared"
             False -> do
                     offset <- addSymbol varName
                     adjustment <- stackPointerValue
                     case value of
                          Nothing     -> return $ loadValue 0
                                                  ++ varOnStack offset
                                                  ++ (adjustStackPointer $ negate adjustment)
                          Just value  -> genASM value

genASM (AssignmentNode varName value operator) = do
        currScope <- currentScope
        offset <- findOffset currScope varName
        assign <- genASM value
        adjustment <- stackPointerValue
        return $ assign
                 ++ varOnStack offset
                 ++ (adjustStackPointer $ negate adjustment)

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
                 ++ testResult
                 ++ (emitJump JE failLabel)
                 ++ passAction
                 ++ (emitJump JMP passLabel)
                 ++ (emitLabel failLabel)
                 ++ failAction
                 ++ (emitLabel passLabel)

genASM (BinaryNode left right op) = do
        lft <- genASM left
        rgt <- genASM right
        return $ binary lft rgt op

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        return $ unode ++ (unary op)

genASM (VarNode varName) = do
        currScope <- currentScope
        offset <- findOffset currScope varName
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


adjustStackPointer :: Int -> String
adjustStackPointer offset =
        "movq %rbp, %rsp\n"
        ++ "subq $" ++ (show offset) ++ ", %rsp\n"


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
   | o == Modulo             = loadTwoValues loadVal2 loadVal1 ++ "cqto\n"
                                                               ++ "idivq %rcx\n"
                                                               ++ "movq %rdx, %rax\n"
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


emitJump :: Jump -> Int -> String
emitJump j n
        | j == JMP  = "jmp _label_" ++ (show n) ++ "\n"
        | j == JE   = "je _label_" ++ (show n) ++ "\n"
        | otherwise = error "Unrecognised type of jump"


emitLabel :: Int -> String
emitLabel n = "_label_" ++ (show n) ++ ":\n"


testResult :: String
testResult = "cmpq $0, %rax\n"


hasReturn :: [Tree] -> Bool
hasReturn blockItems =
        case length blockItems of
             0                  -> False
             _ ->
                     case last blockItems of
                          (ReturnNode val) -> True
                          _                -> False
