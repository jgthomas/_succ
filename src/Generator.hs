
module Generator (generate) where


import Data.Maybe    (isNothing)
import Control.Monad (when, unless)

import AST           (Tree(..))
import Tokens        (Operator(..))
import ASM_Tokens    (Jump(..))
import Error         (CompilerError(..), SyntaxError(..))
import GenState      (GenState)
import Types         (mkSymTab)
import SuccState     (runSuccState, throwError)
import qualified     SymTab
import qualified     ASM
import qualified     TypeCheck


generate :: Tree -> Either CompilerError String
generate ast = runSuccState genASM ast mkSymTab


genASM :: Tree -> GenState String

genASM (ProgramNode topLevelItems) = do
        text   <- concat <$> mapM genASM topLevelItems
        bss    <- ASM.allUninitialized <$> SymTab.getUndefined
        toInit <- ASM.outputInit . concat <$> SymTab.getAllForInit
        return $ text ++ bss ++ toInit

genASM node@(FunctionNode _ _ _ Nothing) = do
        declareFunction node
        return ASM.noOutput
genASM node@(FunctionNode _ name _ (Just stmts)) = do
        checkIfFuncDefined node
        declareFunction node
        SymTab.initFunction name
        statements <- mapM genASM stmts
        SymTab.closeFunction
        SymTab.defineFunction name
        if hasReturn stmts || name /= "main"
           then return $ ASM.functionName name
                         ++ concat statements
           else return $ ASM.functionName name
                         ++ concat statements
                         ++ ASM.loadValue 0
                         ++ ASM.returnStatement


genASM (ParamNode typ (VarNode name)) = do
        SymTab.addParameter name typ
        return ASM.noOutput
genASM node@(ParamNode _ _) = throwError $ SyntaxError (Unexpected node)

genASM node@(FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        checkArguments paramCount node
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
           else throwError $ SyntaxError (Undeclared node)

genASM (ArgNode arg) = genASM arg

genASM (CompoundStmtNode blockItems) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        return . concat $ blockLines

genASM (ForLoopNode ini test iter block) = do
        SymTab.initScope
        passLabel     <- SymTab.labelNum
        failLabel     <- SymTab.labelNum
        continueLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue continueLabel
        inits <- genASM ini
        tests <- genASM test
        iters <- genASM iter
        body  <- genASM block
        SymTab.closeScope
        return $ inits
                 ++ ASM.emitLabel passLabel
                 ++ tests
                 ++ ASM.testResult
                 ++ ASM.emitJump JE failLabel
                 ++ body
                 ++ ASM.emitLabel continueLabel
                 ++ iters
                 ++ ASM.emitJump JMP passLabel
                 ++ ASM.emitLabel failLabel

genASM (WhileNode test whileBlock) = do
        loopLabel <- SymTab.labelNum
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        body      <- genASM whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        return $ ASM.emitLabel loopLabel
                 ++ tests
                 ++ ASM.testResult
                 ++ ASM.emitJump JE testLabel
                 ++ body
                 ++ ASM.emitJump JMP loopLabel
                 ++ ASM.emitLabel testLabel

genASM (DoWhileNode block test) = do
        loopLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        body      <- genASM block
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        return $ ASM.emitLabel loopLabel
                 ++ body
                 ++ ASM.emitLabel contLabel
                 ++ tests
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

genASM (PointerNode varName typ Nothing) =
        genASM (DeclarationNode varName typ Nothing)
genASM node@(PointerNode varName typ (Just a)) = do
        pointerASM <- genASM (DeclarationNode varName typ Nothing)
        value      <- genASM a
        (offset, _, globLab) <- checkVariableExists varName
        if isNothing offset && isNothing globLab
           then throwError $ SyntaxError (Undeclared node)
           else return $ pointerASM ++ value ++ storeAddressOf offset globLab

genASM node@(DeclarationNode varName typ value) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then declareGlobal node
           else do
                   checkIfUsedInScope varName
                   offset <- SymTab.addVariable varName typ
                   adjust <- SymTab.stackPointerValue
                   case value of
                        Just v  -> genASM v
                        Nothing -> return $ ASM.loadValue 0
                                            ++ ASM.varOnStack offset
                                            ++ ASM.adjustStackPointer adjust

genASM node@(AssignmentNode varName value op) = do
        TypeCheck.assignment varName value
        currScope <- SymTab.currentScope
        if currScope == "global"
           then defineGlobal node
           else do
                   (offset, _, globLab) <- checkVariableExists varName
                   assign  <- buildAssignmentASM (VarNode varName) value op
                   if isNothing offset && isNothing globLab
                      then throwError $ SyntaxError (Undefined node)
                      else do
                              varToAssign <- assignToVariable offset globLab
                              return $ assign ++ varToAssign

genASM node@(AssignDereferenceNode varName value op) = do
        TypeCheck.assignment varName value
        assign  <- buildAssignmentASM (DereferenceNode varName) value op
        (offset, argPos, globLab) <- checkVariableExists varName
        if isNothing offset && isNothing argPos && isNothing globLab
           then throwError $ SyntaxError (Undeclared node)
           else return $ assign ++ storeAtDereferenced offset argPos globLab

genASM (ExprStmtNode expression) = genASM expression

genASM ContinueNode = do
        continueLabel <- SymTab.getContinue
        case continueLabel of
             Just target -> return $ ASM.emitJump JMP target
             Nothing     -> throwError $ SyntaxError (Unexpected ContinueNode)

genASM BreakNode = do
        breakLabel <- SymTab.getBreak
        case breakLabel of
             Just target -> return $ ASM.emitJump JMP target
             Nothing     -> throwError $ SyntaxError (Unexpected BreakNode)

genASM (ReturnNode tree) = do
        TypeCheck.funcReturn tree
        rtn <- genASM tree
        return $ rtn ++ ASM.returnStatement

genASM (TernaryNode cond pass fails) = do
        testVal    <- genASM cond
        passAction <- genASM pass
        failAction <- genASM fails
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
        (offset, argPos, globLab) <- checkVariableExists varName
        return $ getVariableASM offset argPos globLab

genASM node@(AddressOfNode varName) = do
        (offset, _, globLab) <- checkVariableExists varName
        if isNothing offset && isNothing globLab
           then throwError $ SyntaxError (Undeclared node)
           else return $ loadAddressOf offset globLab

genASM node@(DereferenceNode varName) = do
        (offset, argPos, globLab) <- checkVariableExists varName
        if isNothing offset && isNothing argPos && isNothing globLab
           then throwError $ SyntaxError (Undeclared node)
           else return $ loadDereferenced offset argPos globLab

genASM NullExprNode = return ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.currentScope
        if currScope == "global"
           then return . show $ n
           else return . ASM.loadValue $ n


-- Global variables

declareGlobal :: Tree -> GenState String
declareGlobal node@(DeclarationNode name typ toAssign) = do
        checkIfFunction node
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _ -> do
                     TypeCheck.globalDeclaration name typ
                     genAssignment toAssign
             Nothing  -> do
                     labnum <- SymTab.labelNum
                     let globLab = mkGlobLabel name labnum
                     SymTab.declareGlobal name typ globLab
                     genAssignment toAssign
declareGlobal tree = throwError $ SyntaxError (Unexpected tree)


checkIfFunction :: Tree -> GenState ()
checkIfFunction node@(DeclarationNode name _ _) = do
        paramNum <- SymTab.decParamCount name
        unless (isNothing paramNum) $
            throwError $ SyntaxError (DoubleDeclared node)
checkIfFunction tree = throwError $ SyntaxError (Unexpected tree)


genAssignment :: Maybe Tree -> GenState String
genAssignment toAssign =
        case toAssign of
             Nothing     -> return ASM.noOutput
             Just assign -> genASM assign


mkGlobLabel :: String -> Int -> String
mkGlobLabel name labnum = "_" ++ name ++ show labnum


defineGlobal :: Tree -> GenState String
defineGlobal node@(AssignmentNode name valNode _) = do
        checkIfDefined node
        label <- SymTab.globalLabel name
        case label of
             Nothing  -> throwError $ SyntaxError (Undeclared node)
             Just lab -> do
                     SymTab.defineGlobal name
                     value <- genASM valNode
                     case valNode of
                          (ConstantNode _)  -> return $ globalVarASM lab value
                          (AddressOfNode _) -> do
                                  SymTab.storeForInit $ value ++ ASM.varAddressStoreGlobal lab
                                  return $ ASM.uninitializedGlobal lab
                          _ -> undefined
defineGlobal tree = throwError $ SyntaxError (Unexpected tree)


checkIfDefined :: Tree -> GenState ()
checkIfDefined node@(AssignmentNode name _ _) = do
        defined <- SymTab.checkVarDefined name
        when defined $
           throwError $ SyntaxError (DoubleDefined node)
checkIfDefined tree = throwError $ SyntaxError (Unexpected tree)


globalVarASM :: String -> String -> String
globalVarASM lab con
        | con == "0" = ASM.uninitializedGlobal lab
        | otherwise  = ASM.initializedGlobal lab con


-- Functions / function calls

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode typ funcName paramList _) = do
        checkIfVariable node
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing    -> do
                     SymTab.declareFunction typ funcName (length paramList)
                     processParameters funcName paramList
             Just count -> do
                     checkCountsMatch count node
                     TypeCheck.paramDeclaration funcName paramList
                     TypeCheck.funcTypeDeclaration funcName typ
                     SymTab.declareFunction typ funcName (length paramList)
                     defined <- SymTab.checkFuncDefined funcName
                     unless defined $
                        do SymTab.delFuncState funcName
                           processParameters funcName paramList
declareFunction tree = throwError $ SyntaxError (Unexpected tree)


checkIfVariable :: Tree -> GenState ()
checkIfVariable node@(FunctionNode _ name _ _) = do
        label <- SymTab.globalLabel name
        unless (isNothing label) $
            throwError $ SyntaxError (DoubleDefined node)
checkIfVariable tree = throwError $ SyntaxError (Unexpected tree)


checkCountsMatch :: Int -> Tree -> GenState ()
checkCountsMatch count node@(FunctionNode _ _ paramList _) =
        when (count /= length paramList) $
           throwError $ SyntaxError (MisMatch count node)
checkCountsMatch count node@(FuncCallNode _ argList) =
        when (count /= length argList) $
           throwError $ SyntaxError (MisMatch count node)
checkCountsMatch _ tree = throwError $ SyntaxError (Unexpected tree)


checkArguments :: Maybe Int -> Tree -> GenState ()
checkArguments (Just n) node@(FuncCallNode _ _) = checkCountsMatch n node
checkArguments (Just _) tree = throwError $ SyntaxError (Unexpected tree)
checkArguments Nothing tree  = throwError $ SyntaxError (Undeclared tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ genASM params
        SymTab.closeFunction


hasReturn :: [Tree] -> Bool
hasReturn items
        | null items = False
        | otherwise  = case last items of
                            ReturnNode _ -> True
                            _            -> False


processArgs :: [Tree] -> Int -> [String] -> GenState String
processArgs argList argPos argASM
        | null argList = return $ concat argASM
        | otherwise    = do
                asm <- processArg argPos $ head argList
                processArgs (tail argList) (argPos+1) (argASM ++ [asm])


processArg :: Int -> Tree -> GenState String
processArg argPos arg = do
        argASM <- genASM arg
        return $ argASM ++ (ASM.putInRegister . ASM.selectRegister $ argPos)


validSequence :: Maybe Int -> Maybe Int -> Bool
validSequence Nothing (Just _) = error "callee undefined"
validSequence (Just _) Nothing = error "caller undefined"
validSequence Nothing Nothing  = error "callee and caller undefined"
validSequence (Just callee) (Just caller)
        | callee <= caller = True
        | otherwise        = False


checkIfFuncDefined :: Tree -> GenState ()
checkIfFuncDefined node@(FunctionNode _ name _ _) = do
        defined <- SymTab.checkFuncDefined name
        when defined $
           throwError $ SyntaxError (DoubleDefined node)
checkIfFuncDefined tree = throwError $ SyntaxError (Unexpected tree)


-- Variables

checkVariableExists :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
checkVariableExists varName = do
        offset  <- SymTab.variableOffset varName
        argPos  <- SymTab.parameterPosition varName
        globLab <- SymTab.globalLabel varName
        return (offset, argPos, globLab)


buildAssignmentASM :: Tree -> Tree -> Operator -> GenState String
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
storeAtDereferenced _ _ _ = error "variable unrecognised"


loadDereferenced :: Maybe Int -> Maybe Int -> Maybe String -> String
loadDereferenced (Just off) _ _ = ASM.derefLoadLocal off
loadDereferenced _ (Just pos) _ = ASM.derefLoadParam pos
loadDereferenced _ _ (Just lab) = ASM.derefLoadGlobal lab
loadDereferenced _ _ _ = error "variable unrecognised"


loadAddressOf :: Maybe Int -> Maybe String -> String
loadAddressOf (Just off) _ = ASM.varAddressLoad off
loadAddressOf _ (Just lab) = ASM.varAddressLoadGlobal lab
loadAddressOf _ _ = error "address unrecognised"


storeAddressOf :: Maybe Int -> Maybe String -> String
storeAddressOf (Just off) _ = ASM.varAddressStore off
storeAddressOf _ (Just _)   = ASM.noOutput
storeAddressOf _ _ = error "address unrecognised"


assignToVariable :: Maybe Int -> Maybe String -> GenState String
assignToVariable (Just off) _ = do
        adjustment <- SymTab.stackPointerValue
        return $ ASM.varOnStack off ++ ASM.adjustStackPointer adjustment
assignToVariable _ (Just lab) = return $ ASM.storeGlobal lab
assignToVariable _ _ = error "address unrecognised"


checkIfUsedInScope :: String -> GenState ()
checkIfUsedInScope name = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           error $ "already declared in scope: '" ++ name
