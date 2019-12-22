{-|
Module       : Generator
Description  : Produces assembly code

Generates the x86-64 assembly code for a particular abstract syntax tree.
-}
module Generator (generate) where


import           Control.Monad (unless, when)
import           Data.Maybe    (isNothing)

import qualified ASM
import           AST           (Tree (..))
import           Error         (CompilerError (SyntaxError), SyntaxError (..))
import           GenState      (GenState, mkSymTab)
import           GenTokens     (Jump (..), Scope (..))
import           Operator      (BinaryOp (..))
import           SuccState     (runSuccState, throwError)
import qualified SymTab
import qualified TypeCheck


-- | Generate x86-64 asm from AST
generate :: Tree -> Either CompilerError String
generate ast = runSuccState genASM ast mkSymTab


genASM :: Tree -> GenState String

genASM (ProgramNode topLevelItems) = do
        text   <- concat <$> mapM genASM topLevelItems
        bss    <- ASM.allUninitialized <$> SymTab.getUndefined
        toInit <- ASM.outputInit . concat <$> SymTab.getAllForInit
        pure $ text ++ bss ++ toInit

genASM node@(FunctionNode _ _ _ Nothing) = do
        declareFunction node
        pure ASM.noOutput
genASM node@(FunctionNode _ name _ (Just stmts)) = do
        checkIfFuncDefined node
        declareFunction node
        SymTab.initFunction name
        statements <- mapM genASM stmts
        SymTab.closeFunction
        SymTab.defineFunction name
        if hasReturn stmts || name /= "main"
           then pure $ ASM.functionName name
                         ++ concat statements
           else pure $ ASM.functionName name
                         ++ concat statements
                         ++ ASM.loadValue 0
                         ++ ASM.returnStatement


genASM (ParamNode typ (VarNode name)) = do
        SymTab.addParameter name typ
        pure ASM.noOutput
genASM node@(ParamNode _ _) = throwError $ SyntaxError (Unexpected node)

genASM node@(FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        checkArguments paramCount node
        TypeCheck.typesMatch name argList
        validateCall node
        argString <- processArgs argList 0 []
        pure $ ASM.saveCallerRegisters
                 ++ argString
                 ++ ASM.makeFunctionCall name
                 ++ ASM.restoreCallerRegisters

genASM (ArgNode arg) = genASM arg

genASM (CompoundStmtNode blockItems) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        pure . concat $ blockLines

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
        pure $ inits
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
        pure $ ASM.emitLabel loopLabel
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
        pure $ ASM.emitLabel loopLabel
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
             Nothing -> pure $ ifLines ++ ASM.emitLabel label
             Just e  -> do
                     elseAction <- genASM e
                     nextLabel  <- SymTab.labelNum
                     pure $ ifLines
                              ++ ASM.emitJump JMP nextLabel
                              ++ ASM.emitLabel label
                              ++ elseAction ++ ASM.emitLabel nextLabel

genASM (PointerNode varName typ Nothing) =
        genASM (DeclarationNode varName typ Nothing)
genASM node@(PointerNode varName typ (Just a)) = do
        pointerASM <- genASM (DeclarationNode varName typ Nothing)
        value      <- genASM a
        (offset, _, globLab) <- checkVariableExists varName
        case (offset, globLab) of
             (Just off, _) -> pure $ pointerASM ++ value ++ ASM.varAddressStore off
             (_, Just _)   -> pure $ pointerASM ++ value
             _ -> throwError $ SyntaxError (Unrecognised node)

genASM node@(DeclarationNode varName typ value) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> declareGlobal node
             Local  -> do
                   checkIfUsedInScope node
                   offset <- SymTab.addVariable varName typ
                   adjust <- SymTab.stackPointerValue
                   case value of
                        Just v  -> genASM v
                        Nothing -> pure $ ASM.loadValue 0
                                            ++ ASM.varOnStack offset
                                            ++ ASM.adjustStackPointer adjust

genASM node@(AssignmentNode varName value op) = do
        TypeCheck.assignment varName value
        currScope <- SymTab.getScope
        case currScope of
             Global -> defineGlobal node
             Local  -> do
                     assign  <- buildAssignmentASM (VarNode varName) value op
                     (offset, _, globLab) <- checkVariableExists varName
                     case (offset, globLab) of
                          (Just off, _) -> do
                                  adj <- SymTab.stackPointerValue
                                  pure $ assign
                                         ++ ASM.varOnStack off
                                         ++ ASM.adjustStackPointer adj
                          (_, Just lab) -> pure $ assign
                                                  ++ ASM.storeGlobal lab
                          _ -> throwError $ SyntaxError (Undeclared node)

genASM node@(AssignDereferenceNode varName value op) = do
        TypeCheck.assignment varName value
        assign <- buildAssignmentASM (DereferenceNode varName) value op
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Just off, _, _) -> pure $ assign ++ ASM.derefStoreLocal off
             (_, Just pos, _) -> pure $ assign ++ ASM.derefStoreParam pos
             (_, _, Just lab) -> pure $ assign ++ ASM.derefStoreGlobal lab
             _                -> throwError $ SyntaxError (Undeclared node)

genASM (ExprStmtNode expression) = genASM expression

genASM ContinueNode = do
        continueLabel <- SymTab.getContinue
        case continueLabel of
             Just target -> pure $ ASM.emitJump JMP target
             Nothing     -> throwError $ SyntaxError (Unexpected ContinueNode)

genASM BreakNode = do
        breakLabel <- SymTab.getBreak
        case breakLabel of
             Just target -> pure $ ASM.emitJump JMP target
             Nothing     -> throwError $ SyntaxError (Unexpected BreakNode)

genASM (ReturnNode tree) = do
        TypeCheck.funcReturn tree
        rtn <- genASM tree
        pure $ rtn ++ ASM.returnStatement

genASM (TernaryNode cond pass fails) = do
        testExp  <- genASM cond
        true     <- genASM pass
        false    <- genASM fails
        falseLab <- SymTab.labelNum
        ASM.ternary testExp true false falseLab <$> SymTab.labelNum

genASM (BinaryNode left right op) = do
        lab1 <- SymTab.labelNum
        lab2 <- SymTab.labelNum
        lft  <- genASM left
        rgt  <- genASM right
        pure $ ASM.binary lft rgt op lab1 lab2

genASM (UnaryNode tree op) = do
        unode <- genASM tree
        pure $ unode ++ ASM.unary op

genASM node@(VarNode varName) = do
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Just off, _, _) -> pure . ASM.varOffStack $ off
             (_, Just reg, _) -> pure . ASM.getFromRegister . ASM.selectRegister $ reg
             (_, _, Just lab) -> pure . ASM.loadGlobal $ lab
             _ -> throwError $ SyntaxError (Unrecognised node)

genASM node@(AddressOfNode varName) = do
        (offset, _, globLab) <- checkVariableExists varName
        case (offset, globLab) of
             (Just off, _) -> pure . ASM.varAddressLoad $ off
             (_, Just lab) -> pure . ASM.varAddressLoadGlobal $ lab
             _             -> throwError $ SyntaxError (Unrecognised node)

genASM node@(DereferenceNode varName) = do
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Just off, _, _) -> pure . ASM.derefLoadLocal $ off
             (_, Just pos, _) -> pure . ASM.derefLoadParam $ pos
             (_, _, Just lab) -> pure . ASM.derefLoadGlobal $ lab
             _                -> throwError $ SyntaxError (Unrecognised node)

genASM NullExprNode = pure ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> pure . show $ n
             Local  -> pure . ASM.loadValue $ n


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
genAssignment Nothing  = pure ASM.noOutput
genAssignment (Just t) = genASM t


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
                          (ConstantNode _)  -> pure $ globalVarASM lab value
                          (AddressOfNode _) -> do
                                  SymTab.storeForInit $ value ++ ASM.varAddressStoreGlobal lab
                                  pure $ ASM.uninitializedGlobal lab
                          _ -> undefined
defineGlobal tree = throwError $ SyntaxError (Unexpected tree)


checkIfDefined :: Tree -> GenState ()
checkIfDefined node@(AssignmentNode name _ _) = do
        defined <- SymTab.checkVarDefined name
        when defined $
           throwError $ SyntaxError (DoubleDefined node)
checkIfDefined tree = throwError $ SyntaxError (Unexpected tree)


globalVarASM :: String -> String -> String
globalVarASM lab "0" = ASM.uninitializedGlobal lab
globalVarASM lab val = ASM.initializedGlobal lab val


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
                     TypeCheck.typesMatch funcName paramList
                     TypeCheck.funcDeclaration funcName typ
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
hasReturn [] = False
hasReturn items =
        case last items of
             ReturnNode _ -> True
             _            -> False


processArgs :: [Tree] -> Int -> [String] -> GenState String
processArgs argList argPos argASM
        | null argList = pure $ concat argASM
        | otherwise    = do
                asm <- processArg argPos $ head argList
                processArgs (tail argList) (argPos+1) (argASM ++ [asm])


processArg :: Int -> Tree -> GenState String
processArg argPos arg = do
        argASM <- genASM arg
        pure $ argASM ++ (ASM.putInRegister . ASM.selectRegister $ argPos)


validateCall :: Tree -> GenState ()
validateCall node@(FuncCallNode name _) = do
        callee <- SymTab.decSeqNumber name
        caller <- SymTab.currentSeqNumber
        unless (validSeq callee caller) $
            throwError $ SyntaxError (InvalidCall node)
validateCall tree = throwError $ SyntaxError (Unexpected tree)


validSeq :: Maybe Int -> Maybe Int -> Bool
validSeq Nothing Nothing   = False
validSeq Nothing (Just _)  = False
validSeq (Just _) Nothing  = False
validSeq (Just a) (Just b) = a <= b


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
        pure (offset, argPos, globLab)


buildAssignmentASM :: Tree -> Tree -> BinaryOp -> GenState String
buildAssignmentASM _ valTree Assignment = genASM valTree
buildAssignmentASM varTree valTree binOp =
        genASM (BinaryNode varTree valTree binOp)


checkIfUsedInScope :: Tree -> GenState ()
checkIfUsedInScope node@(DeclarationNode name _ _) = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           throwError $ SyntaxError (DoubleDeclared node)
checkIfUsedInScope tree = throwError $ SyntaxError (Unexpected tree)
