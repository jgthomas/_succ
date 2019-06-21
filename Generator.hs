
module Generator (genASM) where

import AST        (Tree(..))
import Evaluator  (Evaluator)
import Tokens     (Operator(..))
import ASM_Tokens (Jump(..))
import Types      (Type(..))
import qualified  SymTab
import qualified  ASM


genASM :: Tree -> Evaluator String

genASM (ProgramNode topLevelItems) = do
        prog  <- mapM genASM topLevelItems
        undef <- SymTab.getUndefined
        let bss = map ASM.uninitializedGlobal undef
        return $ concat prog ++ concat bss

genASM (FunctionProtoNode name paramList) = do
        declareFunction name paramList
        return ASM.noOutput

genASM (FunctionNode name paramList statementList) = do
        defined <- SymTab.checkFuncDefined name
        if defined
           then error $ "Function aleady defined: " ++ name
           else do
                   declareFunction name paramList
                   SymTab.initFunction name
                   statements <- mapM genASM statementList
                   SymTab.closeFunction
                   SymTab.defineFunction name
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
        checkArguments paramCount name argList
        checkTypesMatch name argList
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
                                            ++ ASM.varAddressStore off
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
                   globLab <- SymTab.globalLabel varName
                   assign <- buildAssignmentASM (VarNode varName) value op
                   if offset == Nothing && globLab == Nothing
                      then error $ "Undefined variable: " ++ varName
                      else do
                              varToAssign <- assignToVariable offset globLab
                              return $ assign ++ varToAssign

genASM (AssignDereferenceNode varName value op) = do
        assign <- buildAssignmentASM (DereferenceNode varName) value op
        offset <- SymTab.variableOffset varName
        argPos <- SymTab.parameterPosition varName
        if offset == Nothing && argPos == Nothing
           then error $ "variable not declared: " ++ varName
           else return $ assign ++ storeAtDereferenced offset argPos

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
             Just off -> return . ASM.varAddressLoad $ off

genASM (DereferenceNode varName) = do
        offset <- SymTab.variableOffset varName
        argPos <- SymTab.parameterPosition varName
        if offset == Nothing && argPos == Nothing
           then error $ "variable not declared: " ++ varName
           else return $ loadDereferenced offset argPos

genASM (NullExprNode) = return ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then return . show $ n
           else return . ASM.loadValue $ n


-- Global variables

declareGlobal :: String -> Type -> Maybe Tree -> Evaluator String
declareGlobal name typ toAssign = do
        checkIfFunction name
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just lab -> genAssignment toAssign
             Nothing  -> do
                     labnum <- SymTab.labelNum
                     let globLab = mkGlobLabel name labnum
                     SymTab.declareGlobal name typ globLab
                     genAssignment toAssign


checkIfFunction :: String -> Evaluator ()
checkIfFunction name = do
        paramNum <- SymTab.decParamCount name
        case paramNum of
             Nothing -> return ()
             Just n  -> error $ "already declared as function: " ++ name


genAssignment :: Maybe Tree -> Evaluator String
genAssignment toAssign = do
        case toAssign of
             Nothing     -> return ASM.noOutput
             Just assign -> genASM assign


mkGlobLabel :: String -> Int -> String
mkGlobLabel name labnum = "_" ++ name ++ show labnum


defineGlobal :: String -> Tree -> Evaluator String
defineGlobal name constNode = do
        checkIfDefined name
        const <- genASM constNode
        label <- SymTab.globalLabel name
        case label of
             Nothing  -> error $ "variable not declared: " ++ name
             Just lab -> do
                     SymTab.defineGlobal name
                     if read const == 0
                        then return . ASM.uninitializedGlobal $ lab
                        else return . ASM.initializedGlobal lab $ const


checkIfDefined :: String -> Evaluator ()
checkIfDefined name = do
        defined <- SymTab.checkVarDefined name
        if defined
           then error $ "global variable already defined: " ++ name
           else return ()


-- Functions / function calls

declareFunction :: String -> [Tree] -> Evaluator ()
declareFunction funcName paramList = do
        checkIfVariable funcName
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing    -> do
                     SymTab.declareFunction funcName (length paramList)
                     processParameters funcName paramList
             Just count -> do
                     checkCountsMatch count funcName paramList
                     checkTypesMatch funcName paramList
                     SymTab.declareFunction funcName (length paramList)
                     defined <- SymTab.checkFuncDefined funcName
                     if not defined
                        then do
                                SymTab.delFuncState funcName
                                processParameters funcName paramList
                        else return ()


checkIfVariable :: String -> Evaluator ()
checkIfVariable name = do
        label <- SymTab.globalLabel name
        case label of
             Nothing -> return ()
             Just l  -> error $ "already defined as variable: " ++ name


checkCountsMatch :: Int -> String -> [Tree] -> Evaluator ()
checkCountsMatch count name paramList =
        if count /= length paramList
           then error $ "mismatch in parameter/argument counts for: " ++ name
           else return ()


checkArguments :: Maybe Int -> String -> [Tree] -> Evaluator ()
checkArguments Nothing name argList  = error $ "called function not declared: " ++ name
checkArguments (Just n) name argList = checkCountsMatch n name argList


processParameters :: String -> [Tree] -> Evaluator ()
processParameters name params = do
        SymTab.initFunction name
        mapM genASM params
        SymTab.closeFunction


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


validSequence :: Maybe Int -> Maybe Int -> Bool
validSequence Nothing (Just caller) = error "callee undefined"
validSequence (Just callee) Nothing = error "caller undefined"
validSequence (Just callee) (Just caller)
        | callee <= caller = True
        | otherwise        = False


-- Variable assignment

buildAssignmentASM :: Tree -> Tree -> Operator -> Evaluator String
buildAssignmentASM varTree valueTree op
        | op == Assign         = genASM valueTree
        | op == PlusAssign     = genASM (BinaryNode varTree valueTree Plus)
        | op == MinusAssign    = genASM (BinaryNode varTree valueTree Minus)
        | op == MultiplyAssign = genASM (BinaryNode varTree valueTree Multiply)
        | op == DivideAssign   = genASM (BinaryNode varTree valueTree Divide)
        | op == ModuloAssign   = genASM (BinaryNode varTree valueTree Modulo)
        | otherwise            = error $ "unrecognised assignment operator: " ++ show op


getVariableASM :: Maybe Int -> Maybe Int -> Maybe String -> String
getVariableASM (Just off) _ _ = ASM.varOffStack off
getVariableASM _ (Just reg) _ = ASM.getFromRegister . ASM.selectRegister $ reg
getVariableASM _ _ (Just lab) = ASM.loadGlobal lab
getVariableASM Nothing Nothing Nothing = error "variable unrecognised"


storeAtDereferenced :: Maybe Int -> Maybe Int -> String
storeAtDereferenced (Just off) _ = ASM.derefStoreLocal off
storeAtDereferenced _ (Just pos) = ASM.derefStoreParam pos


loadDereferenced :: Maybe Int -> Maybe Int -> String
loadDereferenced (Just off) _ = ASM.derefLoadLocal off
loadDereferenced _ (Just pos) = ASM.derefLoadParam pos


assignToVariable :: Maybe Int -> Maybe String -> Evaluator String
assignToVariable (Just off) _ = do
        adjustment <- SymTab.stackPointerValue
        return $ ASM.varOnStack off ++ ASM.adjustStackPointer adjustment
assignToVariable _ (Just lab) = do
        return $ ASM.storeGlobal lab


-- Type checking

checkTypesMatch :: String -> [Tree] -> Evaluator ()
checkTypesMatch name paramList = do
        currTypes <- SymTab.allTypes name
        newTypes  <- mapM getType paramList
        if currTypes == newTypes
           then return ()
           else error $ "mismatching types for parameters/arguments: "
                        ++ show currTypes ++ " vs. "
                        ++ show newTypes ++ " "
                        ++ "for function: " ++ name


getType :: Tree -> Evaluator Type
getType (ArgNode tree)       = getType tree
getType (ParamNode typ tree) = return typ
getType (VarNode name)       = getVariableType name
getType (AddressOfNode name) = return IntPointer
getType _                    = return IntVar


getVariableType :: String -> Evaluator Type
getVariableType name = do
        typ <- SymTab.variableType name
        case typ of
             Just t  -> return t
             Nothing -> error $ "no type associated with variable: " ++ name
