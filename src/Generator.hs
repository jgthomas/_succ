
module Generator (genASM) where

import Data.Maybe    (isNothing)
import Control.Monad (when, unless)

import AST        (Tree(..))
import Evaluator  (Evaluator)
import Tokens     (Operator(..))
import ASM_Tokens (Jump(..))
import Types      (Type(..))
import qualified  SymTab
import qualified  ASM
import qualified  TypeCheck


genASM :: Tree -> Evaluator String

genASM (ProgramNode topLevelItems) = do
        text   <- concat <$> mapM genASM topLevelItems
        bss    <- ASM.allUninitialized <$> SymTab.getUndefined
        toInit <- ASM.outputInit . concat <$> SymTab.getAllForInit
        return $ text ++ bss ++ toInit

genASM (FunctionNode typ name paramList statementList) = do
        case statementList of
             Nothing -> do
                     declareFunction typ name paramList
                     return ASM.noOutput
             Just statementList -> do
                     defined <- SymTab.checkFuncDefined name
                     if defined
                        then error $ "Function aleady defined: " ++ name
                        else do
                                declareFunction typ name paramList
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
        TypeCheck.argsMatchParams name argList
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

genASM (ArgNode arg) = genASM arg

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
             Nothing -> return pointerASM
             Just a  -> do
                     value   <- genASM a
                     offset  <- SymTab.variableOffset varName
                     globLab <- SymTab.globalLabel varName
                     if isNothing offset && isNothing globLab
                        then error $ "variable not declared: " ++ varName
                        else return $ pointerASM ++ value ++ storeAddressOf offset globLab

genASM (DeclarationNode varName typ value) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then declareGlobal varName typ value
           else do
                   checkIfUsedInScope varName
                   offset <- SymTab.addVariable varName typ
                   adjust <- SymTab.stackPointerValue
                   case value of
                        Just v  -> genASM v
                        Nothing -> return $ ASM.loadValue 0
                                            ++ ASM.varOnStack offset
                                            ++ ASM.adjustStackPointer adjust

genASM (AssignmentNode varName value op) = do
        TypeCheck.assignment varName value
        currScope <- SymTab.currentScope
        if currScope == "global"
           then defineGlobal varName value
           else do
                   offset  <- SymTab.variableOffset varName
                   globLab <- SymTab.globalLabel varName
                   assign  <- buildAssignmentASM (VarNode varName) value op
                   if isNothing offset && isNothing globLab
                      then error $ "Undefined variable: " ++ varName
                      else do
                              varToAssign <- assignToVariable offset globLab
                              return $ assign ++ varToAssign

genASM (AssignDereferenceNode varName value op) = do
        TypeCheck.assignment varName value
        assign  <- buildAssignmentASM (DereferenceNode varName) value op
        offset  <- SymTab.variableOffset varName
        argPos  <- SymTab.parameterPosition varName
        globLab <- SymTab.globalLabel varName
        if isNothing offset && isNothing argPos && isNothing globLab
           then error $ "variable not declared: " ++ varName
           else return $ assign ++ storeAtDereferenced offset argPos globLab

genASM (ExprStmtNode expression) = genASM expression

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
        TypeCheck.funcReturn tree
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
        offset  <- SymTab.variableOffset varName
        globLab <- SymTab.globalLabel varName
        if isNothing offset && isNothing globLab
           then error $ "trying to get address of undeclared variable: " ++ varName
           else return $ loadAddressOf offset globLab

genASM (DereferenceNode varName) = do
        offset  <- SymTab.variableOffset varName
        argPos  <- SymTab.parameterPosition varName
        globLab <- SymTab.globalLabel varName
        if isNothing offset && isNothing argPos && isNothing globLab
           then error $ "trying to dereference undeclared variable: " ++ varName
           else return $ loadDereferenced offset argPos globLab

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
             Just lab -> do
                     TypeCheck.globalDeclaration name typ
                     genAssignment toAssign
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
defineGlobal name valNode = do
        checkIfDefined name
        label <- SymTab.globalLabel name
        case label of
             Nothing  -> error $ "variable not declared: " ++ name
             Just lab -> do
                     SymTab.defineGlobal name
                     value <- genASM valNode
                     case valNode of
                          (ConstantNode n)  -> return $ globalVarASM lab value
                          (AddressOfNode a) -> do
                                  SymTab.storeForInit $ value ++ ASM.varAddressStoreGlobal lab
                                  return $ ASM.uninitializedGlobal lab


checkIfDefined :: String -> Evaluator ()
checkIfDefined name = do
        defined <- SymTab.checkVarDefined name
        when defined $
           error $ "global variable already defined: " ++ name


globalVarASM :: String -> String -> String
globalVarASM lab const =
        if read const == 0
           then ASM.uninitializedGlobal lab
           else ASM.initializedGlobal lab const


-- Functions / function calls

declareFunction :: Type -> String -> [Tree] -> Evaluator ()
declareFunction typ funcName paramList = do
        checkIfVariable funcName
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing    -> do
                     SymTab.declareFunction typ funcName (length paramList)
                     processParameters funcName paramList
             Just count -> do
                     checkCountsMatch count funcName paramList
                     TypeCheck.paramDeclaration funcName paramList
                     TypeCheck.funcTypeDeclaration funcName typ
                     SymTab.declareFunction typ funcName (length paramList)
                     defined <- SymTab.checkFuncDefined funcName
                     unless defined $
                        do SymTab.delFuncState funcName
                           processParameters funcName paramList


checkIfVariable :: String -> Evaluator ()
checkIfVariable name = do
        label <- SymTab.globalLabel name
        case label of
             Nothing -> return ()
             Just l  -> error $ "already defined as variable: " ++ name


checkCountsMatch :: Int -> String -> [Tree] -> Evaluator ()
checkCountsMatch count name paramList =
        when (count /= length paramList) $
           error $ "mismatch in parameter/argument counts for: " ++ name


checkArguments :: Maybe Int -> String -> [Tree] -> Evaluator ()
checkArguments Nothing name argList  = error $ "called function not declared: " ++ name
checkArguments (Just n) name argList = checkCountsMatch n name argList


processParameters :: String -> [Tree] -> Evaluator ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ genASM params
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


-- Variables

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


storeAtDereferenced :: Maybe Int -> Maybe Int -> Maybe String -> String
storeAtDereferenced (Just off) _ _ = ASM.derefStoreLocal off
storeAtDereferenced _ (Just pos) _ = ASM.derefStoreParam pos
storeAtDereferenced _ _ (Just lab) = ASM.derefStoreGlobal lab


loadDereferenced :: Maybe Int -> Maybe Int -> Maybe String -> String
loadDereferenced (Just off) _ _ = ASM.derefLoadLocal off
loadDereferenced _ (Just pos) _ = ASM.derefLoadParam pos
loadDereferenced _ _ (Just lab) = ASM.derefLoadGlobal lab


loadAddressOf :: Maybe Int -> Maybe String -> String
loadAddressOf (Just off) _ = ASM.varAddressLoad off
loadAddressOf _ (Just lab) = ASM.varAddressLoadGlobal lab


storeAddressOf :: Maybe Int -> Maybe String -> String
storeAddressOf (Just off) _ = ASM.varAddressStore off
storeAddressOf _ (Just lab) = ASM.noOutput


assignToVariable :: Maybe Int -> Maybe String -> Evaluator String
assignToVariable (Just off) _ = do
        adjustment <- SymTab.stackPointerValue
        return $ ASM.varOnStack off ++ ASM.adjustStackPointer adjustment
assignToVariable _ (Just lab) = do
        return $ ASM.storeGlobal lab


checkIfUsedInScope :: String -> Evaluator ()
checkIfUsedInScope name = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           error $ "already declared in scope: '" ++ name
