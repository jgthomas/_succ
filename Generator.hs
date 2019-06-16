
module Generator (genASM) where

import AST        (Tree(..))
import Evaluator  (Evaluator)
import Tokens     (Operator(..))
import ASM_Tokens (Jump(..))
import Types      (Type)
import qualified  SymTab
import qualified  ASM


genASM :: Tree -> Evaluator String

genASM (ProgramNode topLevelItems) = do
        prog  <- mapM genASM topLevelItems
        undef <- SymTab.getUndefined
        let bss = map ASM.uninitializedGlobal undef
        return $ concat prog ++ concat bss

genASM (FunctionProtoNode name paramList) = do
        declareFunction name $ length paramList
        return ASM.noOutput

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
                      then return $ ASM.functionName name
                                    ++ concat statements
                      else return $ ASM.functionName name
                                    ++ concat statements
                                    ++ ASM.loadValue 0
                                    ++ ASM.returnStatement

genASM (ParamNode typ param) = do
       case param of
            VarNode name -> do
                    SymTab.addParameter name typ
                    return ASM.noOutput
            _ -> error $ "Invalid parameter: " ++ show param

genASM (FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        checkArguments paramCount (length argList) name
        callee <- SymTab.decSeqNumber name
        caller <- SymTab.currentSeqNumber
        if validSequence callee caller
           then do
                   argString <- processArgs argList 0 []
                   return $ ASM.saveCallerRegisters
                            ++ argString
                            ++ ASM.makeFunctionCall name
                            ++ ASM.restoreCallerRegisters
           else error $ "calling function before declaration " ++ name

genASM (ArgNode arg) = do
        argAsm <- genASM arg
        return argAsm

genASM (CompoundStmtNode blockItems) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        return . concat $ blockLines

genASM (ForLoopNode init test iter block) = do
        SymTab.initScope
        passLabel     <- SymTab.labelNum
        failLabel     <- SymTab.labelNum
        continueLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue continueLabel
        init <- genASM init
        test <- genASM test
        iter <- genASM iter
        body <- genASM block
        SymTab.closeScope
        return $ init
                 ++ ASM.emitLabel passLabel
                 ++ test
                 ++ ASM.testResult
                 ++ ASM.emitJump JE failLabel
                 ++ body
                 ++ ASM.emitLabel continueLabel
                 ++ iter
                 ++ ASM.emitJump JMP passLabel
                 ++ ASM.emitLabel failLabel

genASM (WhileNode test whileBlock) = do
        loopLabel <- SymTab.labelNum
        test      <- genASM test
        testLabel <- SymTab.labelNum
        body      <- genASM whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        return $ ASM.emitLabel loopLabel
                 ++ test
                 ++ ASM.testResult
                 ++ ASM.emitJump JE testLabel
                 ++ body
                 ++ ASM.emitJump JMP loopLabel
                 ++ ASM.emitLabel testLabel

genASM (DoWhileNode block test) = do
        loopLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        body      <- genASM block
        test      <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        return $ ASM.emitLabel loopLabel
                 ++ body
                 ++ ASM.emitLabel contLabel
                 ++ test
                 ++ ASM.testResult
                 ++ ASM.emitJump JE testLabel
                 ++ ASM.emitJump JMP loopLabel
                 ++ ASM.emitLabel testLabel

genASM (IfNode test action possElse) = do
        testVal  <- genASM test
        ifAction <- genASM action
        label    <- SymTab.labelNum
        let ifLines = testVal
                      ++ ASM.testResult
                      ++ ASM.emitJump JE label
                      ++ ifAction
        case possElse of
             Nothing -> return $ ifLines ++ ASM.emitLabel label
             Just e  -> do
                     elseAction <- genASM e
                     nextLabel  <- SymTab.labelNum
                     return $ ifLines
                              ++ ASM.emitJump JMP nextLabel
                              ++ ASM.emitLabel label
                              ++ elseAction ++ ASM.emitLabel nextLabel

genASM (PointerNode varName typ assign) = do
        pointerASM <- genASM (DeclarationNode varName typ Nothing)
        case assign of
             Just a -> do
                     value  <- genASM a
                     offset <- SymTab.variableOffset varName
                     case offset of
                          Nothing  -> error $ "variable not declared: " ++ varName
                          Just off -> return $ pointerASM
                                            ++ value
                                            ++ (ASM.varAddressStore . ASM.relAddress $ off)
             _ -> return pointerASM

genASM (DeclarationNode varName typ value) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then declareGlobal varName typ value
           else do
                   localDec <- SymTab.checkVariable varName
                   paramDec <- SymTab.parameterDeclared varName
                   if localDec || paramDec
                      then error $ "already declared in scope: '" ++ varName
                      else do
                              offset <- SymTab.addVariable varName typ
                              adjust <- SymTab.stackPointerValue
                              case value of
                                   Just v  ->
                                           genASM v
                                   Nothing ->
                                           return $ ASM.loadValue 0
                                                    ++ ASM.varOnStack offset
                                                    ++ ASM.adjustStackPointer adjust

genASM (AssignmentNode varName value op) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then defineGlobal varName value
           else do
                   offset <- SymTab.variableOffset varName
                   assign <- buildAssignmentASM (VarNode varName) value op
                   case offset of
                        Just off -> do
                                adjustment <- SymTab.stackPointerValue
                                return $ assign
                                         ++ ASM.varOnStack off
                                         ++ ASM.adjustStackPointer adjustment
                        Nothing  -> do
                                globLab <- SymTab.globalLabel varName
                                case globLab of
                                     Just lab -> return $ assign
                                                          ++ ASM.storeGlobal lab
                                     Nothing  -> error $ "Undefined variable: "
                                                         ++ varName

genASM (AssignDereferenceNode varName value op) = do
        assign <- buildAssignmentASM (DereferenceNode varName) value op
        offset <- SymTab.variableOffset varName
        case offset of
             Just off -> return $ assign ++ ASM.derefStoreLocal off
             Nothing  -> do
                     argPos <- SymTab.parameterPosition varName
                     case argPos of
                          Just pos -> return $ assign ++ ASM.derefStoreParam pos
                          Nothing  -> error $ "variable not declared: " ++ varName

genASM (ExprStmtNode expression) = do
        exprsn <- genASM expression
        return exprsn

genASM (ContinueNode) = do
        continueLabel <- SymTab.getContinue
        case continueLabel of
             Just target -> return $ ASM.emitJump JMP target
             Nothing     -> error "Continue statement outside loop"

genASM (BreakNode) = do
        breakLabel <- SymTab.getBreak
        case breakLabel of
             Just target -> return $ ASM.emitJump JMP target
             Nothing     -> error "Break statement outside loop"

genASM (ReturnNode tree) = do
        rtn <- genASM tree
        return $ rtn ++ ASM.returnStatement

genASM (TernaryNode cond pass fail) = do
        testVal    <- genASM cond
        passAction <- genASM pass
        failAction <- genASM fail
        failLabel  <- SymTab.labelNum
        passLabel  <- SymTab.labelNum
        return $ testVal
                 ++ ASM.testResult
                 ++ ASM.emitJump JE failLabel
                 ++ passAction
                 ++ ASM.emitJump JMP passLabel
                 ++ ASM.emitLabel failLabel
                 ++ failAction
                 ++ ASM.emitLabel passLabel

genASM (BinaryNode left right op) = do
        nextLabel <- SymTab.labelNum
        endLabel  <- SymTab.labelNum
        lft <- genASM left
        rgt <- genASM right
        case op of
             LogicalOR  ->
                     return $ ASM.logicalOR lft rgt nextLabel endLabel
             LogicalAND ->
                     return $ ASM.logicalAND lft rgt nextLabel endLabel
             _          ->
                     return $ ASM.binary lft rgt op

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        return $ unode ++ ASM.unary op

genASM (VarNode varName) = do
        offset <- SymTab.variableOffset varName
        argPos <- SymTab.parameterPosition varName
        label  <- SymTab.globalLabel varName
        return $ getVariableASM offset argPos label

genASM (AddressOfNode varName) = do
        offset <- SymTab.variableOffset varName
        case offset of
             Nothing  -> error $ "variable not declared: " ++ varName
             Just off -> return . ASM.varAddressLoad . ASM.relAddress $ off

genASM (DereferenceNode varName) = do
        offset <- SymTab.variableOffset varName
        case offset of
             Just off -> return . ASM.derefLoadLocal $ off
             Nothing  -> do
                     argPos <- SymTab.parameterPosition varName
                     case argPos of
                          Just pos -> return . ASM.derefLoadParam $ pos
                          Nothing  -> error $ "variable not declared: " ++ varName

genASM (NullExprNode) = return ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then return . show $ n
           else return . ASM.loadValue $ n


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
hasReturn items
        | length items == 0 = False
        | otherwise = case last items of
                           ReturnNode val -> True
                           _              -> False


processArgs :: [Tree] -> Int -> [String] -> Evaluator String
processArgs argList argPos argASM = do
        if length argList == 0
           then return $ concat argASM
           else do
                   asm <- processArg argPos $ head argList
                   processArgs (tail argList) (argPos+1) (argASM ++ [asm])


processArg :: Int -> Tree -> Evaluator String
processArg argPos arg = do
        argASM <- genASM arg
        return $ argASM ++ (ASM.putInRegister . ASM.selectRegister $ argPos)


declareGlobal :: String -> Type -> Maybe Tree -> Evaluator String
declareGlobal name typ toAssign = do
        existsFunc <- funcDec name
        if existsFunc
           then error $ "'" ++ name ++ "' already declared as function"
           else do currLabel  <- SymTab.globalLabel name
                   case currLabel of
                        Just lab -> genAssignment toAssign
                        Nothing  -> do
                                labnum <- SymTab.labelNum
                                let globLab = mkGlobLabel name labnum
                                SymTab.declareGlobal name typ globLab
                                genAssignment toAssign


genAssignment :: Maybe Tree -> Evaluator String
genAssignment toAssign = do
        case toAssign of
             Nothing     -> return ASM.noOutput
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
                        Nothing  -> error $ "variable not declared: " ++ name
                        Just lab -> do
                                SymTab.defineGlobal name
                                if read const == 0
                                   then return . ASM.uninitializedGlobal $ lab
                                   else return . ASM.initializedGlobal lab $ const


mkGlobLabel :: String -> Int -> String
mkGlobLabel name labnum = "_" ++ name ++ show labnum


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


buildAssignmentASM :: Tree -> Tree -> Operator -> Evaluator String
buildAssignmentASM varTree valueTree op
        | op == Assign         = genASM valueTree
        | op == PlusAssign     = genASM (BinaryNode varTree valueTree Plus)
        | op == MinusAssign    = genASM (BinaryNode varTree valueTree Minus)
        | op == MultiplyAssign = genASM (BinaryNode varTree valueTree Multiply)
        | op == DivideAssign   = genASM (BinaryNode varTree valueTree Divide)
        | op == ModuloAssign   = genASM (BinaryNode varTree valueTree Modulo)
        | otherwise            = error $ "unrecognised assignment operator: " ++ show op


validSequence :: Maybe Int -> Maybe Int -> Bool
validSequence Nothing (Just caller) = error "callee undefined"
validSequence (Just callee) Nothing = error "caller undefined"
validSequence (Just callee) (Just caller)
        | callee <= caller = True
        | otherwise        = False


getVariableASM :: Maybe Int -> Maybe Int -> Maybe String -> String
getVariableASM (Just off) _ _ = ASM.varOffStack off
getVariableASM _ (Just reg) _ = ASM.getFromRegister . ASM.selectRegister $ reg
getVariableASM _ _ (Just lab) = ASM.loadGlobal lab
getVariableASM Nothing Nothing Nothing = error "variable unrecognised"


