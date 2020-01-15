{-|
Module       : Generator
Description  : Produces assembly code

Generates the x86-64 assembly code for a particular abstract syntax tree.
-}
module Generator (generate) where


import           Control.Monad       (unless)
import           Control.Monad.Extra (concatMapM)

import qualified ASM
import           AST                 (Tree (..))
import           Error               (CompilerError (GeneratorError, SyntaxError),
                                      GeneratorError (..), SyntaxError (..))
import           GenState            (GenState, runGenState, throwError)
import qualified GenState            (startState)
import           GenTokens           (Scope (..))
import           Operator            (BinaryOp (..), Operator (..),
                                      UnaryOp (..))
import qualified SymTab


-- | Generate x86-64 asm from AST
generate :: Tree -> Either CompilerError String
generate ast = runGenState genASM ast GenState.startState


genASM :: Tree -> GenState String

genASM (ProgramNode topLevelItems) = do
        text  <- concatMapM genASM topLevelItems
        undef <- SymTab.getUndefined
        bss   <- concatMapM ASM.uninitializedGlobal undef
        toIni <- ASM.outputInit . concat <$> SymTab.getAllForInit
        pure $ text ++ bss ++ toIni

genASM node@(FunctionNode _ _ _ Nothing) = do
        declareFunction node
        ASM.noOutput
genASM node@(FunctionNode _ name _ (Just stmts)) = do
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
genASM node@(ParamNode _ _) =
        throwError $ SyntaxError (Unexpected node)

genASM (FuncCallNode name args) =
        ASM.functionCall name <$> processArgs args

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
        (offset, _, globLab) <- SymTab.getVariables varName
        case (offset, globLab) of
             (Just off, _) -> ASM.varAddressStore (pointerASM ++ value) off
             (_, Just _)   -> pure $ pointerASM ++ value
             _             -> throwError $ SyntaxError (Unrecognised node)

genASM node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> declareGlobal node
             Local  -> declareLocal node

genASM node@AssignmentNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> defineGlobal node
             Local  -> defineLocal node

genASM (AssignDereferenceNode varName value op) = do
        assign <- buildAssignmentASM (DereferenceNode varName) value op
        (offset, argPos, globLab) <- SymTab.getVariables varName
        ASM.derefStore assign offset argPos globLab

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

genASM (ReturnNode tree) = ASM.returnValue <$> genASM tree

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
        (off, _, lab) <- SymTab.getVariables a
        ASM.unary unaryASM op off lab
genASM (UnaryNode _ unOp@(PreOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
genASM (UnaryNode _ unOp@(PostOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
genASM (UnaryNode tree (Unary op)) = do
        unode <- genASM tree
        ASM.unary unode (Unary op) Nothing Nothing

genASM (VarNode name) = do
        (offset, argPos, globLab) <- SymTab.getVariables name
        ASM.loadVariable offset argPos globLab

genASM (AddressOfNode name) = do
        (offset, _, globLab) <- SymTab.getVariables name
        ASM.addressOf offset globLab

genASM (DereferenceNode name) = do
        (offset, argPos, globLab) <- SymTab.getVariables name
        ASM.derefLoad offset argPos globLab

genASM NullExprNode = ASM.noOutput

genASM (ConstantNode n) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> pure . show $ n
             Local  -> ASM.loadLiteral n


-- Global variables

declareGlobal :: Tree -> GenState String
declareGlobal (DeclarationNode name typ toAssign) = do
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> genAssignment toAssign
             Nothing -> do
                     globLab <- SymTab.mkGlobLabel name
                     SymTab.declareGlobal name typ globLab
                     genAssignment toAssign
declareGlobal tree = throwError $ SyntaxError (Unexpected tree)


genAssignment :: Maybe Tree -> GenState String
genAssignment Nothing     = ASM.noOutput
genAssignment (Just tree) = genASM tree


defineGlobal :: Tree -> GenState String
defineGlobal node@(AssignmentNode name _ _) = do
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


-- Local variables

declareLocal :: Tree -> GenState String
declareLocal (DeclarationNode varName typ value) = do
        offset <- SymTab.addVariable varName typ
        adjust <- SymTab.stackPointerValue
        case value of
             Just val -> genASM val
             Nothing  -> ASM.decNoAssign offset adjust
declareLocal tree = throwError $ SyntaxError (Unexpected tree)


defineLocal :: Tree -> GenState String
defineLocal node@(AssignmentNode varName value op) = do
        assign <- buildAssignmentASM (VarNode varName) value op
        (offset, _, globLab) <- SymTab.getVariables varName
        case (offset, globLab) of
             (Just off, _) -> ASM.assign assign off <$> SymTab.stackPointerValue
             (_, Just lab) -> ASM.storeGlobal assign lab
             _ -> throwError $ SyntaxError (Undeclared node)
defineLocal tree = throwError $ SyntaxError (Unexpected tree)


-- Functions / function calls

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode _ funcName _ _) = do
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> declareNewFunction node
             Just _  -> declareRepeatFunction node
declareFunction tree = throwError $ SyntaxError (Unexpected tree)


declareNewFunction :: Tree -> GenState ()
declareNewFunction (FunctionNode typ funcName paramList _) = do
        SymTab.declareFunction typ funcName (length paramList)
        processParameters funcName paramList
declareNewFunction tree = throwError $ SyntaxError (Unexpected tree)


declareRepeatFunction :: Tree -> GenState ()
declareRepeatFunction (FunctionNode typ funcName paramList _) = do
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ SyntaxError (Unexpected tree)


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
processArgs args = concatMapM processArg (zip args [0..])


processArg :: (Tree, Int) -> GenState String
processArg (arg, pos) = do
        argASM <- genASM arg
        ASM.passArgument argASM pos


-- Variables

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
