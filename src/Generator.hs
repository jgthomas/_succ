{-|
Module       : Generator
Description  : Produces assembly code

Generates the x86-64 assembly code for a particular abstract syntax tree.
-}
module Generator (generate) where


import           Control.Monad (unless)

import qualified ASM
import           AST           (Tree (..))
import           Error         (CompilerError (GeneratorError, SyntaxError),
                                GeneratorError (..), SyntaxError (..))
import           GenState      (GenState, runGenState, throwError)
import qualified GenState      (startState)
import           GenTokens     (Scope (..))
import           Operator      (BinaryOp (..), Operator (..), UnaryOp (..))
import qualified SymTab
import qualified TypeCheck
import qualified Validate      as Valid


-- | Generate x86-64 asm from AST
generate :: Tree -> Either CompilerError String
generate ast = runGenState genASM ast GenState.startState


genASM :: Tree -> GenState String

genASM (ProgramNode topLevelItems) = do
        text  <- concat <$> mapM genASM topLevelItems
        undef <- SymTab.getUndefined
        bss   <- concat <$> mapM ASM.uninitializedGlobal undef
        toIni <- concat <$> SymTab.getAllForInit
        doIni <- ASM.outputInit toIni
        pure $ text ++ bss ++ doIni

genASM node@(FunctionNode _ _ _ Nothing) = do
        declareFunction node
        ASM.noOutput
genASM node@(FunctionNode _ name _ (Just stmts)) = do
        Valid.checkIfFuncDefined node
        declareFunction node
        SymTab.initFunction name
        statements <- concat <$> mapM genASM stmts
        SymTab.closeFunction
        SymTab.defineFunction name
        if hasReturn stmts || name /= "main"
           then ASM.function name statements
           else ASM.mainNoReturn name statements

genASM (ParamNode typ (VarNode name)) = do
        SymTab.addParameter name typ
        ASM.noOutput
genASM node@(ParamNode _ _) = throwError $ SyntaxError (Unexpected node)

genASM node@(FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        Valid.checkArguments paramCount node
        TypeCheck.typesMatch name argList
        Valid.validateCall node
        argString <- processArgs argList
        ASM.functionCall name argString

genASM (ArgNode arg) = genASM arg

genASM (CompoundStmtNode blockItems) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        pure . concat $ blockLines

genASM (ForLoopNode ini test iter block) = do
        SymTab.initScope
        passLabel <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue contLabel
        inits <- genASM ini
        tests <- genASM test
        iters <- genASM iter
        body  <- genASM block
        SymTab.closeScope
        ASM.forLoop inits tests iters body passLabel failLabel contLabel

genASM (WhileNode test whileBlock) = do
        loopLabel <- SymTab.labelNum
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        body      <- genASM whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        ASM.while tests body loopLabel testLabel

genASM (DoWhileNode block test) = do
        loopLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        body      <- genASM block
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        ASM.doWhile body tests loopLabel contLabel testLabel

genASM (IfNode test action possElse) = do
        testExp <- genASM test
        ifAct   <- genASM action
        label   <- SymTab.labelNum
        case possElse of
             Nothing -> ASM.ifOnly testExp ifAct label
             Just e  -> do
                     elseAct <- genASM e
                     nextLab <- SymTab.labelNum
                     ASM.ifElse testExp ifAct label elseAct nextLab

genASM (PointerNode varName typ Nothing) =
        genASM (DeclarationNode varName typ Nothing)
genASM node@(PointerNode varName typ (Just a)) = do
        pointerASM <- genASM (DeclarationNode varName typ Nothing)
        value      <- genASM a
        (offset, _, globLab) <- checkVariableExists varName
        case (offset, globLab) of
             (Just off, _) -> ASM.varAddressStore (pointerASM ++ value) off
             (_, Just _)   -> pure $ pointerASM ++ value
             _             -> throwError $ SyntaxError (Unrecognised node)

genASM node@(DeclarationNode varName typ value) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> declareGlobal node
             Local  -> do
                   Valid.checkIfUsedInScope node
                   offset <- SymTab.addVariable varName typ
                   adjust <- SymTab.stackPointerValue
                   case value of
                        Just val -> genASM val
                        Nothing  -> ASM.decNoAssign offset adjust

genASM node@(AssignmentNode varName value op) = do
        TypeCheck.assignment varName value
        currScope <- SymTab.getScope
        case currScope of
             Global -> defineGlobal node
             Local  -> do
                     assign <- buildAssignmentASM (VarNode varName) value op
                     (offset, _, globLab) <- checkVariableExists varName
                     case (offset, globLab) of
                          (Just off, _) -> do
                                  adj <- SymTab.stackPointerValue
                                  ASM.assign assign off adj
                          (_, Just lab) -> ASM.storeGlobal assign lab
                          _ -> throwError $ SyntaxError (Undeclared node)

genASM node@(AssignDereferenceNode varName value op) = do
        TypeCheck.assignment varName value
        assign <- buildAssignmentASM (DereferenceNode varName) value op
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Nothing, Nothing, Nothing) ->
                     throwError $ SyntaxError (Undeclared node)
             _ -> ASM.derefStore assign offset argPos globLab

genASM (ExprStmtNode expression) = genASM expression

genASM ContinueNode = do
        continueLabel <- SymTab.getContinue
        case continueLabel of
             Just target -> ASM.setGotoPoint target
             Nothing     -> throwError $ SyntaxError (Unexpected ContinueNode)

genASM BreakNode = do
        breakLabel <- SymTab.getBreak
        case breakLabel of
             Just target -> ASM.setGotoPoint target
             Nothing     -> throwError $ SyntaxError (Unexpected BreakNode)

genASM (ReturnNode tree) = do
        TypeCheck.funcReturn tree
        rtn <- genASM tree
        ASM.returnValue rtn

genASM (TernaryNode cond pass fails) = do
        testExp  <- genASM cond
        true     <- genASM pass
        false    <- genASM fails
        trueLab  <- SymTab.labelNum
        falseLab <- SymTab.labelNum
        ASM.ternary testExp true false trueLab falseLab

genASM node@(BinaryNode _ (ConstantNode n) (ShiftOp _)) =
        processBinaryNode node (show n)
genASM node@(BinaryNode _ right _) = do
        rgt <- genASM right
        processBinaryNode node rgt

genASM (UnaryNode (VarNode a) op) = do
        unaryASM      <- genASM (VarNode a)
        (off, _, lab) <- checkVariableExists a
        ASM.unary unaryASM op off lab
genASM (UnaryNode _ unOp@(PreOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
genASM (UnaryNode _ unOp@(PostOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
genASM (UnaryNode tree (Unary op)) = do
        unode <- genASM tree
        ASM.unary unode (Unary op) Nothing Nothing

genASM node@(VarNode varName) = do
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Nothing, Nothing, Nothing) ->
                     throwError $ SyntaxError (Unrecognised node)
             _ -> ASM.loadVariable offset argPos globLab

genASM node@(AddressOfNode varName) = do
        (offset, _, globLab) <- checkVariableExists varName
        case (offset, globLab) of
             (Nothing, Nothing) ->
                     throwError $ SyntaxError (Unrecognised node)
             _ -> ASM.addressOf offset globLab

genASM node@(DereferenceNode varName) = do
        (offset, argPos, globLab) <- checkVariableExists varName
        case (offset, argPos, globLab) of
             (Nothing, Nothing, Nothing) ->
                     throwError $ SyntaxError (Unrecognised node)
             _ -> ASM.derefLoad offset argPos globLab

genASM NullExprNode = ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> pure . show $ n
             Local  -> ASM.loadLiteral n


-- Global variables

declareGlobal :: Tree -> GenState String
declareGlobal node@(DeclarationNode name typ toAssign) = do
        Valid.checkIfFunction node
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> do
                     TypeCheck.globalDeclaration name typ
                     genAssignment toAssign
             Nothing -> do
                     globLab <- Valid.mkGlobLabel name <$> SymTab.labelNum
                     SymTab.declareGlobal name typ globLab
                     genAssignment toAssign
declareGlobal tree = throwError $ SyntaxError (Unexpected tree)


genAssignment :: Maybe Tree -> GenState String
genAssignment Nothing     = ASM.noOutput
genAssignment (Just tree) = genASM tree


--mkGlobLabel :: String -> Int -> String
--mkGlobLabel name labnum = "_" ++ name ++ show labnum


defineGlobal :: Tree -> GenState String
defineGlobal node@(AssignmentNode name _ _) = do
        Valid.checkIfDefined node
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        defPrevDecGlob label node
defineGlobal tree = throwError $ SyntaxError (Unexpected tree)


defPrevDecGlob :: Maybe String -> Tree -> GenState String
defPrevDecGlob Nothing node = throwError $ SyntaxError (Undeclared node)
defPrevDecGlob (Just label) (AssignmentNode _ (ConstantNode a) _) = do
        value <- genASM (ConstantNode a)
        globalVarASM label value
defPrevDecGlob (Just label) (AssignmentNode _ (AddressOfNode a) _) = do
        value   <- genASM (AddressOfNode a)
        initASM <- ASM.varAddressStoreGlobal value label
        SymTab.storeForInit initASM
        ASM.uninitializedGlobal label
defPrevDecGlob _ (AssignmentNode _ valNode _) =
        throwError $ SyntaxError (Unexpected valNode)
defPrevDecGlob _ tree = throwError $ SyntaxError (Unexpected tree)


globalVarASM :: String -> String -> GenState String
globalVarASM lab "0" = ASM.uninitializedGlobal lab
globalVarASM lab val = ASM.initializedGlobal lab val


-- Functions / function calls

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode typ funcName paramList _) = do
        Valid.checkIfVariable node
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing    -> do
                     SymTab.declareFunction typ funcName (length paramList)
                     processParameters funcName paramList
             Just count -> do
                     Valid.checkCountsMatch count node
                     TypeCheck.typesMatch funcName paramList
                     TypeCheck.funcDeclaration funcName typ
                     SymTab.declareFunction typ funcName (length paramList)
                     defined <- SymTab.checkFuncDefined funcName
                     unless defined $
                        do SymTab.delFuncState funcName
                           processParameters funcName paramList
declareFunction tree = throwError $ SyntaxError (Unexpected tree)


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


processArgs :: [Tree] -> GenState String
processArgs args = concat <$> mapM processArg (zip args [0..])


processArg :: (Tree, Int) -> GenState String
processArg (arg, pos) = do
        argASM <- genASM arg
        ASM.passArgument argASM pos


-- Variables

checkVariableExists :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
checkVariableExists varName = do
        offset  <- SymTab.variableOffset varName
        argPos  <- SymTab.parameterPosition varName
        globLab <- SymTab.globalLabel varName
        pure (offset, argPos, globLab)


buildAssignmentASM :: Tree -> Tree -> Operator -> GenState String
buildAssignmentASM _ valTree Assignment = genASM valTree
buildAssignmentASM varTree valTree (BinaryOp binOp) =
        genASM (BinaryNode varTree valTree binOp)
buildAssignmentASM _ _ (UnaryOp a) =
        throwError $ GeneratorError (OperatorError (UnaryOp a))


-- Operators

processBinaryNode :: Tree -> String -> GenState String
processBinaryNode (BinaryNode left _ op) rgt = do
        lab1 <- SymTab.labelNum
        lab2 <- SymTab.labelNum
        lft  <- genASM left
        ASM.binary lft rgt op lab1 lab2
processBinaryNode tree _ = throwError $ SyntaxError (Unexpected tree)
