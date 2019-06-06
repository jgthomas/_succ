
module Generator (genASM) where

import AST       (Tree(..))
import Evaluator (Evaluator)
import Tokens    (Operator(..))
import qualified SymTab


genASM :: Tree -> Evaluator String

genASM (ProgramNode topLevelItems) = do
        prog  <- mapM genASM topLevelItems
        undef <- SymTab.getUndefined
        let bss = map uninitializedGlobal undef
        return $ concat prog ++ concat bss

genASM (FunctionProtoNode name paramList) = do
        declareFunction name (length paramList)
        return ""

genASM (FunctionNode name paramList statementList) = do
        defined <- SymTab.functionDefined name
        if defined
           then error $ "Function aleady defined: " ++ name
           else do
                   declareFunction name $ length paramList
                   SymTab.initFunction name
                   processParameters paramList
                   statements <- mapM genASM statementList
                   SymTab.closeFunction
                   if hasReturn statementList || name /= "main"
                      then return $ functionName name
                                    ++ concat statements
                      else return $ functionName name
                                    ++ concat statements
                                    ++ loadValue 0
                                    ++ returnStatement

genASM (ParamNode param) = do
       case param of
            VarNode name -> do
                    SymTab.addParameter name
                    return ""
            _ -> error $ "Invalid parameter: " ++ (show param)

genASM (FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        checkArguments paramCount (length argList) name
        callee <- SymTab.decSeqNumber name
        caller <- SymTab.currentSeqNumber
        if validSequence callee caller
           then do
                   argString <- processArgs argList 0 []
                   return $ saveCallerRegisters
                            ++ argString
                            ++ (makeFunctionCall name)
                            ++ restoreCallerRegisters
           else error $ "calling function before declaration " ++ name

genASM (ArgNode arg) = do
        argAsm <- genASM arg
        return argAsm

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
        currScope <- SymTab.currentScope
        if currScope == "global"
           then declareGlobal varName value
           else do
                   localDec <- SymTab.checkVariable varName
                   paramDec <- SymTab.parameterDeclared varName
                   if localDec || paramDec
                      then error $ "already declared in scope: '" ++ varName
                      else do
                              offset <- SymTab.addVariable varName
                              adjust <- SymTab.stackPointerValue
                              case value of
                                   Just v  ->
                                           genASM v
                                   Nothing ->
                                           return $ loadValue 0
                                                    ++ varOnStack offset
                                                    ++ (adjustStackPointer adjust)

genASM (AssignmentNode varName value operator) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then defineGlobal varName value
           else do
                   offset <- SymTab.variableOffset varName
                   assign <- genASM value
                   case offset of
                        Just off -> do
                                adjustment <- SymTab.stackPointerValue
                                return $ assign
                                         ++ varOnStack off
                                         ++ (adjustStackPointer adjustment)
                        Nothing  -> do
                                globLab <- SymTab.globalLabel varName
                                case globLab of
                                     Just lab -> return $ assign ++ storeGlobal lab
                                     Nothing  -> error $ "Undefined variable: '"
                                                         ++ varName

genASM (ExprStmtNode expression) = do
        exprsn <- genASM expression
        return exprsn

genASM (ContinueNode) = do
        continueLabel <- SymTab.getContinue
        case continueLabel of
             Just target -> return $ emitJump JMP target
             Nothing     -> error "Continue statement outside loop"

genASM (BreakNode) = do
        breakLabel <- SymTab.getBreak
        case breakLabel of
             Just target -> return $ emitJump JMP target
             Nothing     -> error "Break statement outside loop"

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
        nextLabel <- SymTab.labelNum
        endLabel <- SymTab.labelNum
        lft <- genASM left
        rgt <- genASM right
        case op of
             LogicalOR  -> return $ logicalOR lft rgt nextLabel endLabel
             LogicalAND -> return $ logicalAND lft rgt nextLabel endLabel
             _          -> return $ binary lft rgt op

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        return $ unode ++ (unary op)

genASM (VarNode varName) = do
        offset <- SymTab.variableOffset varName
        argPos <- SymTab.parameterPosition varName
        label  <- SymTab.globalLabel varName
        return $ getVariableASM offset argPos label

genASM (NullExprNode) = return ""

genASM (ConstantNode n) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then return $ show n
           else return $ loadValue n


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
binary val1 val2 o
   | o == Plus               = loadValues val1 val2 ++ "addq %rcx, %rax\n"
   | o == Multiply           = loadValues val1 val2 ++ "imul %rcx, %rax\n"
   | o == Minus              = loadValues val2 val1 ++ "subq %rcx, %rax\n"
   | o == Divide             = loadValues val2 val1 ++ "cqto\n" ++ "idivq %rcx\n"
   | o == Modulo             = loadValues val2 val1 ++ moduloValues
   | o == Equal              = comparison val1 val2 ++ "sete %al\n"
   | o == NotEqual           = comparison val1 val2 ++ "setne %al\n"
   | o == GreaterThan        = comparison val1 val2 ++ "setg %al\n"
   | o == LessThan           = comparison val1 val2 ++ "setl %al\n"
   | o == GreaterThanOrEqual = comparison val1 val2 ++ "setge %al\n"
   | o == LessThanOrEqual    = comparison val1 val2 ++ "setle %al\n"


logicalOR :: String -> String -> Int -> Int -> String
logicalOR val1 val2 nextLabel endLabel = val1
                       ++ "cmpq $0, %rax\n"
                       ++ emitJump JE nextLabel
                       ++ "movq $1, %rax\n"
                       ++ emitJump JMP endLabel
                       ++ emitLabel nextLabel
                       ++ val2
                       ++ "cmpq $0, %rax\n"
                       ++ "movq $0, %rax\n"
                       ++ "setne %al\n"
                       ++ emitLabel endLabel


logicalAND :: String -> String -> Int -> Int -> String
logicalAND val1 val2 nextLabel endLabel = val1
                       ++ "cmpq $0, %rax\n"
                       ++ emitJump JNE nextLabel
                       ++ emitJump JMP endLabel
                       ++ emitLabel nextLabel
                       ++ val2
                       ++ "cmpq $0, %rax\n"
                       ++ "movq $0, %rax\n"
                       ++ "setne %al\n"
                       ++ emitLabel endLabel


loadValues :: String -> String -> String
loadValues val1 val2 = val1
                    ++ "pushq %rax\n"
                    ++ val2
                    ++ "popq %rcx\n"


comparison :: String -> String -> String
comparison val1 val2 = loadValues val1 val2
                    ++ "cmpq %rax, %rcx\n"
                    ++ "movq $0, %rax\n"


moduloValues :: String
moduloValues = "cqto\n"
            ++ "idivq %rcx\n"
            ++ "movq %rdx, %rax\n"


data Jump = JMP
          | JE
          | JNE
          deriving Eq


emitJump :: Jump -> Int -> String
emitJump j n
        | j == JMP  = "jmp _label_" ++ (show n) ++ "\n"
        | j == JE   = "je _label_" ++ (show n) ++ "\n"
        | j == JNE  = "jne _label_" ++ (show n) ++ "\n"
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


initializedGlobal :: String -> String -> String
initializedGlobal label val =
        ".globl " ++ label ++ "\n"
        ++ ".data\n"
        ++ ".align 4\n"
        ++ label ++ ":\n"
        ++ ".long " ++ val ++ "\n"
        ++ ".text\n"


uninitializedGlobal :: String -> String
uninitializedGlobal label =
        ".globl " ++ label ++ "\n"
        ++ ".bss\n"
        ++ ".align 4\n"
        ++ label ++ ":\n"
        ++ ".text\n"

{-
- gcc treats global labels as position
- independent, PIE, by default, and so as
- relative to %rip, so loads need to be
- from that relative location as well
-}
loadGlobal :: String -> String
loadGlobal label =
        "movq " ++ label ++ "(%rip), %rax\n"


storeGlobal :: String -> String
storeGlobal label =
        "movq %rax, " ++ label ++ "(%rip)\n"


checkArguments :: Maybe Int -> Int -> String -> Evaluator ()
checkArguments Nothing a f = error $ "called function not declared: " ++ f
checkArguments (Just n) a f
       | n /= a    = error $ "argument count does not match parameter count: " ++ f
       | otherwise = return ()


processParameters :: [Tree] -> Evaluator ()
processParameters params = do
        mapM genASM params
        return ()


hasReturn :: [Tree] -> Bool
hasReturn blockItems =
        case length blockItems of
             0                  -> False
             _ ->
                     case last blockItems of
                          (ReturnNode val) -> True
                          _                -> False


processArgs :: [Tree] -> Int -> [String] -> Evaluator String
processArgs argList argPos argASM = do
        if (length argList) == 0
           then return $ concat argASM
           else do
                   asm <- processArg argPos $ head argList
                   processArgs (tail argList) (argPos+1) (argASM ++ [asm])


processArg :: Int -> Tree -> Evaluator String
processArg argPos arg = do
        argASM <- genASM arg
        return $ argASM ++ putInRegister (selectRegister argPos)


declareGlobal :: String -> Maybe Tree -> Evaluator String
declareGlobal name toAssign = do
        existsFunc <- funcDec name
        if existsFunc
           then error $ "'" ++ name ++ "' already declared as function"
           else do currLabel  <- SymTab.globalLabel name
                   case currLabel of
                        Just lab -> genAssignment toAssign
                        Nothing  -> do
                                labnum <- SymTab.labelNum
                                let globLab = mkGlobLabel name labnum
                                SymTab.declareGlobal name globLab
                                genAssignment toAssign


genAssignment :: Maybe Tree -> Evaluator String
genAssignment toAssign = do
        case toAssign of
             Nothing     -> return ""
             Just assign -> genASM assign


funcDec :: String -> Evaluator Bool
funcDec name = do
        paramNum <- SymTab.decParamCount name
        case paramNum of
             Nothing -> return False
             Just n  -> return True


defineGlobal :: String -> Tree -> Evaluator String
defineGlobal name constNode = do
        defined <- SymTab.checkVarDefined name
        if defined
           then error $ "global variable already defined: " ++ name
           else do
                   const <- genASM constNode
                   label <- SymTab.globalLabel name
                   case label of
                        Nothing  -> error $ "variable not yet declared: " ++ name
                        Just lab -> do
                                SymTab.defineGlobal name
                                return $ initializedGlobal lab const


mkGlobLabel :: String -> Int -> String
mkGlobLabel name labnum = "_" ++ name ++ (show labnum)


declareFunction :: String -> Int -> Evaluator ()
declareFunction funcName paramCount = do
        existsVar <- varDec funcName
        if existsVar
           then error $ "'" ++ funcName ++ "' already defined as variable"
           else do prevParamCount <- SymTab.decParamCount funcName
                   case prevParamCount of
                        Nothing -> do SymTab.declareFunction funcName paramCount
                        Just p  -> do
                                if p /= paramCount
                                   then error $ "Mismatch in parameter counts for: " ++ funcName
                                   else do SymTab.declareFunction funcName paramCount


varDec :: String -> Evaluator Bool
varDec name = do
        label <- SymTab.globalLabel name
        case label of
             Nothing -> return False
             Just l  -> return True


validSequence :: Maybe Int -> Maybe Int -> Bool
validSequence Nothing (Just caller) = error "callee undefined"
validSequence (Just callee) Nothing = error "caller undefined"
validSequence (Just callee) (Just caller)
        | callee <= caller = True
        | otherwise        = False


getVariableASM :: Maybe Int -> Maybe Int -> Maybe String -> String
getVariableASM (Just off) _ _ = varOffStack off
getVariableASM _ (Just pos) _ = getFromRegister $ selectRegister pos
getVariableASM _ _ (Just lab) = loadGlobal lab
getVariableASM Nothing Nothing Nothing = error "variable unrecognised"
