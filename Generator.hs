
module Generator (genASM) where

import Tokens (Operator(..))
import AST (Tree(..))
import Evaluator (Evaluator)
import qualified SymTab


genASM :: Tree -> Evaluator String

genASM (ProgramNode functionList) = do
        prog <- mapM genASM functionList
        return $ concat prog

genASM (FunctionProtoNode name paramList) = do
        processDeclaration name (length paramList)
        return ""

genASM (FunctionNode name paramList statementList) = do
        processDeclaration name (length paramList)
        processDefinition name (length paramList)
        SymTab.initFunction name
        paramExpr <- mapM genASM paramList
        funcStmnts <- mapM genASM statementList
        SymTab.closeFunction
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

genASM (ParamNode param) = do
       case param of
            VarNode name -> do
                    SymTab.addParameter name
                    return ""
            _ -> error $ "Invalid parameter: " ++ (show param)

genASM (FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        case paramCount of
             Nothing -> error $ "Called function undefined: " ++ name
             Just p  ->
                     if p /= (length argList)
                        then error $ "Mismatch parameters and arguments: " ++ name
                        else do
                                callee <- SymTab.decSeqNumber name
                                caller <- SymTab.currentSeqNumber
                                if validDefinitionSequence callee caller
                                   then do
                                           argsString <- mapM genASM argList
                                           SymTab.resetArguments
                                           return $ saveCallerRegisters
                                                    ++ concat argsString
                                                    ++ (makeFunctionCall name)
                                                    ++ restoreCallerRegisters
                                   else error "Invalid declaration sequence"

genASM (ArgNode arg) = do
        argAsm <- genASM arg
        argPos <- SymTab.nextArgumentPos
        return $ argAsm ++ putInRegister (selectRegister argPos)

genASM (CompoundStmtNode blockItems) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        return $ concat blockLines

genASM (ForLoopNode init test iter block) = do
        SymTab.initScope
        passLabel <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        continueLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue continueLabel
        init <- genASM init
        test <- genASM test
        iter <- genASM iter
        body <- genASM block
        SymTab.closeScope
        return $ init
                 ++ (emitLabel passLabel)
                 ++ test
                 ++ testResult
                 ++ (emitJump JE failLabel)
                 ++ body
                 ++ (emitLabel continueLabel)
                 ++ iter
                 ++ (emitJump JMP passLabel)
                 ++ (emitLabel failLabel)

genASM (WhileNode test whileBlock) = do
        loopLabel <- SymTab.labelNum
        SymTab.setContinue loopLabel
        test <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setBreak testLabel
        body <- genASM whileBlock
        return $ (emitLabel loopLabel)
                 ++ test
                 ++ testResult
                 ++ (emitJump JE testLabel)
                 ++ body
                 ++ (emitJump JMP loopLabel)
                 ++ (emitLabel testLabel)

genASM (DoWhileNode block test) = do
        loopLabel <- SymTab.labelNum
        continueLabel <- SymTab.labelNum
        SymTab.setContinue continueLabel
        body <- genASM block
        test <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setBreak testLabel
        return $ (emitLabel loopLabel)
                 ++ body
                 ++ (emitLabel continueLabel)
                 ++ test
                 ++ testResult
                 ++ (emitJump JE testLabel)
                 ++ (emitJump JMP loopLabel)
                 ++ (emitLabel testLabel)

genASM (IfNode test action possElse) = do
        testVal <- genASM test
        ifAction <- genASM action
        label <- SymTab.labelNum
        let ifLines = testVal
                      ++ testResult
                      ++ (emitJump JE label)
                      ++ ifAction
        case possElse of
             Nothing       -> return $ ifLines ++ (emitLabel label)
             Just possElse -> do
                     elseAction <- genASM possElse
                     nextLabel <- SymTab.labelNum
                     return $ ifLines
                              ++ (emitJump JMP nextLabel)
                              ++ (emitLabel label)
                              ++ elseAction
                              ++ (emitLabel nextLabel)

genASM (DeclarationNode varName value) = do
        varDeclared <- SymTab.checkVariable varName
        paramDeclared <- SymTab.parameterDeclared varName
        case varDeclared || paramDeclared of
             True  -> error $ "Variable '" ++ varName ++ "' already declared"
             False -> do
                     offset <- SymTab.addVariable varName
                     adjustment <- SymTab.stackPointerValue
                     case value of
                          Nothing     -> return $ loadValue 0
                                                  ++ varOnStack offset
                                                  ++ (adjustStackPointer adjustment)
                          Just value  -> genASM value

genASM (AssignmentNode varName value operator) = do
        offset <- SymTab.variableOffset varName
        if offset /= notFound
           then do
              assign <- genASM value
              adjustment <- SymTab.stackPointerValue
              return $ assign
                       ++ varOnStack offset
                       ++ (adjustStackPointer adjustment)
           else error $ "Undefined variable: '" ++ varName

genASM (ExprStmtNode expression) = do
        exprsn <- genASM expression
        return exprsn

genASM (ContinueNode) = do
        continueLabel <- SymTab.getContinue
        if continueLabel == notFound
           then error "Continue statement outside loop"
           else return $ emitJump JMP continueLabel

genASM (BreakNode) = do
        breakLabel <- SymTab.getBreak
        if breakLabel == notFound
           then error "Break statement outside loop"
           else return $ emitJump JMP breakLabel

genASM (ReturnNode tree) = do
        rtn <- genASM tree
        return $ rtn ++ returnStatement

genASM (TernaryNode cond pass fail) = do
        testVal <- genASM cond
        passAction <- genASM pass
        failAction <- genASM fail
        failLabel <- SymTab.labelNum
        passLabel <- SymTab.labelNum
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
        offset <- SymTab.variableOffset varName
        if offset /= notFound
           then return $ varOffStack offset
           else do
                   argPos <- SymTab.parameterPosition varName
                   if argPos /= notFound
                      then return $ getFromRegister $ selectRegister argPos
                      else error $ "Undefined variable: '" ++ varName

genASM (NullExprNode) = return ""

genASM (ConstantNode n) = return $ loadValue n


functionName :: String -> String
functionName f = ".globl "
                 ++ f
                 ++ "\n"
                 ++ f
                 ++ ":\n"
                 ++ saveBasePointer


returnStatement :: String
returnStatement = restoreBasePointer ++ "ret\n"


saveBasePointer :: String
saveBasePointer = "pushq %rbp\n"
                  ++ "movq %rsp, %rbp\n"


restoreBasePointer :: String
restoreBasePointer = "movq %rbp, %rsp\n"
                     ++ "popq %rbp\n"


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


data Jump = JMP
          | JE
          deriving Eq


emitJump :: Jump -> Int -> String
emitJump j n
        | j == JMP  = "jmp _label_" ++ (show n) ++ "\n"
        | j == JE   = "je _label_" ++ (show n) ++ "\n"
        | otherwise = error "Unrecognised type of jump"


putInRegister :: String -> String
putInRegister reg = "movq %rax, " ++ reg ++ "\n"


getFromRegister :: String -> String
getFromRegister reg = "movq " ++ reg ++ ", %rax\n"


selectRegister :: Int -> String
selectRegister callConvSeq
        | callConvSeq == 0 = "%rdi"
        | callConvSeq == 1 = "%rsi"
        | callConvSeq == 2 = "%rdx"
        | callConvSeq == 3 = "%rcx"
        | callConvSeq == 4 = "%r8"
        | callConvSeq == 5 = "%r9"


makeFunctionCall :: String -> String
makeFunctionCall funcName = "call " ++ funcName ++ "\n"


saveCallerRegisters :: String
saveCallerRegisters =
        "pushq %rdi\n"
        ++ "pushq %rsi\n"
        ++ "pushq %rdx\n"
        ++ "pushq %rcx\n"
        ++ "pushq %r8\n"
        ++ "pushq %r9\n"


restoreCallerRegisters :: String
restoreCallerRegisters =
        "popq %r9\n"
        ++ "popq %r8\n"
        ++ "popq %rcx\n"
        ++ "popq %rdx\n"
        ++ "popq %rsi\n"
        ++ "popq %rdi\n"


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


processDeclaration :: String -> Int -> Evaluator ()
processDeclaration funcName paramCount = do
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> do
                     SymTab.addDeclaration funcName paramCount
                     return ()
             Just p  -> do
                     if p /= paramCount
                        then error $ "Mismatch in parameter counts for: " ++ funcName
                        else return ()


processDefinition :: String -> Int -> Evaluator Bool
processDefinition funcName paramCount = do
        alreadyDefined <- SymTab.functionDefined funcName
        case alreadyDefined of
             False -> do
                     SymTab.addDeclaration funcName paramCount
                     return True
             True  -> error $ "Function aleady defined: " ++ funcName


validDefinitionSequence :: Maybe Int -> Maybe Int -> Bool
validDefinitionSequence Nothing (Just caller) = error "callee undefined"
validDefinitionSequence (Just callee) Nothing = error "caller undefined"
validDefinitionSequence (Just callee) (Just caller)
        | callee <= caller = True
        | otherwise        = False


notFound :: Int
notFound = -1
