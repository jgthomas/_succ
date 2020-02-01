{-|
Module       : Checker
Description  : Check AST for errors

Checks the AST for scope, type and other errors
-}
module Checker (check) where


import           Control.Monad (unless, when)
import           Data.Maybe    (isNothing)

import           AST           (Tree (..))
import           Error         (CompilerError (GeneratorError, ScopeError),
                                GeneratorError (..), ScopeError (..))
import           GenState      (GenState, runGenState, throwError)
import qualified GenState      (startState)
import           GenTokens     (Scope (..))
import           Operator      (Operator (..), UnaryOp (..))
import qualified ScopeCheck
import qualified SymTab
import qualified TypeCheck


-- | Check an AST for errors
check :: Tree -> Either CompilerError Tree
check ast = runGenState checker ast GenState.startState


checker :: Tree -> GenState Tree
checker ast = do
        checkAST ast
        pure ast


checkAST :: Tree -> GenState ()

checkAST (ProgramNode topLevelItems) = mapM_ checkAST topLevelItems

checkAST node@(FunctionNode _ _ _ Nothing _) =
        checkFuncDec node
checkAST node@(FunctionNode _ name _ (Just stmts) _) = do
        ScopeCheck.checkIfFuncDefined node
        checkFuncDec node
        SymTab.initFunction name
        mapM_ checkAST stmts
        SymTab.closeFunction
        SymTab.defineFunction name

checkAST (ParamNode typ (VarNode name)) =
        SymTab.addParameter name typ
checkAST node@ParamNode{} =
        throwError $ ScopeError (UnexpectedNode node)

checkAST node@(FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        ScopeCheck.checkArguments paramCount node
        TypeCheck.typesMatch node
        ScopeCheck.validateCall node
        mapM_ checkAST argList

checkAST (ArgNode arg) = checkAST arg

checkAST (CompoundStmtNode blockItems) = do
        SymTab.initScope
        mapM_ checkAST blockItems
        SymTab.closeScope

checkAST (ForLoopNode ini test iter block) = do
        SymTab.initScope
        _         <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue contLabel
        checkAST ini
        checkAST test
        checkAST iter
        checkAST block
        SymTab.closeScope

checkAST (WhileNode test whileBlock) = do
        loopLabel <- SymTab.labelNum
        checkAST test
        testLabel <- SymTab.labelNum
        checkAST whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel

checkAST (DoWhileNode block test) = do
        _         <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        checkAST block
        checkAST test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel

checkAST (IfNode test action possElse) = do
        checkAST test
        checkAST action
        _ <- SymTab.labelNum
        case possElse of
             Nothing -> pure ()
             Just e  -> do
                     _ <- SymTab.labelNum
                     checkAST e

checkAST (PointerNode varName typ Nothing) =
        checkAST (DeclarationNode varName typ Nothing)
checkAST node@(PointerNode varName typ (Just a)) = do
        checkAST (DeclarationNode varName typ Nothing)
        ScopeCheck.variableExists node
        checkAST a

checkAST node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDeclareGlobal node
             Local  -> checkDeclareLocal node

checkAST node@(AssignmentNode varName value op) = do
        TypeCheck.assignment node
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDefineGlobal node
             Local  -> do
                     ScopeCheck.variableExists node
                     checkAssignLocal (VarNode varName) value op

checkAST node@(AssignDereferenceNode varName value op) = do
        ScopeCheck.variableExists node
        TypeCheck.assignment node
        checkAssignLocal (DereferenceNode varName) value op

checkAST (ExprStmtNode expression) = checkAST expression

checkAST ContinueNode = do
        continueLabel <- SymTab.getContinue
        when (isNothing continueLabel) $
            throwError $ ScopeError (UnexpectedNode ContinueNode)

checkAST BreakNode = do
        breakLabel <- SymTab.getContinue
        when (isNothing breakLabel) $
            throwError $ ScopeError (UnexpectedNode BreakNode)

checkAST node@(ReturnNode tree) = do
        checkAST tree
        TypeCheck.funcReturn node tree

checkAST (TernaryNode cond pass fails) = do
        _ <- SymTab.labelNum
        _ <- SymTab.labelNum
        checkAST cond
        checkAST pass
        checkAST fails

checkAST (BinaryNode lft rgt _) = do
        _ <- SymTab.labelNum
        _ <- SymTab.labelNum
        checkAST lft
        checkAST rgt

checkAST (UnaryNode node@(VarNode _) _) = do
        checkAST node
        ScopeCheck.variableExists node
checkAST (UnaryNode _ unOp@(PreOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
checkAST (UnaryNode _ unOp@(PostOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
checkAST (UnaryNode tree (Unary _)) = checkAST tree

checkAST node@(VarNode _) = ScopeCheck.variableExists node

checkAST node@(AddressOfNode _) = ScopeCheck.variableExists node

checkAST node@(DereferenceNode _) = ScopeCheck.variableExists node

checkAST NullExprNode = pure ()

checkAST (ConstantNode _) = pure ()


checkFuncDec :: Tree -> GenState ()
checkFuncDec node@(FunctionNode _ funcName _ _ _) = do
        ScopeCheck.checkIfVariable node
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> checkNewFuncDec node
             Just ns -> checkRepeatFuncDec ns node
checkFuncDec tree = throwError $ ScopeError (UnexpectedNode tree)


checkNewFuncDec :: Tree -> GenState ()
checkNewFuncDec (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        checkParams funcName paramList
checkNewFuncDec tree = throwError $ ScopeError (UnexpectedNode tree)


checkRepeatFuncDec :: Int -> Tree -> GenState ()
checkRepeatFuncDec count node@(FunctionNode typ funcName paramList _ _) = do
        ScopeCheck.checkCountsMatch count node
        TypeCheck.typesMatch node
        TypeCheck.funcDeclaration node
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $ do
            SymTab.delFuncState funcName
            checkParams funcName paramList
checkRepeatFuncDec _ tree = throwError $ ScopeError (UnexpectedNode tree)


checkParams :: String -> [Tree] -> GenState ()
checkParams name params = do
        SymTab.initFunction name
        mapM_ checkAST params
        SymTab.closeFunction


checkDeclareGlobal :: Tree -> GenState ()
checkDeclareGlobal node@(DeclarationNode name typ toAssign) = do
        ScopeCheck.checkIfFunction node
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> TypeCheck.globalDeclaration node
             Nothing -> do
                     globLab <- SymTab.mkGlobLabel name
                     SymTab.declareGlobal name typ globLab
                     checkAssignment toAssign
checkDeclareGlobal tree = throwError $ ScopeError (UnexpectedNode tree)


checkAssignment :: Maybe Tree -> GenState ()
checkAssignment Nothing  = pure ()
checkAssignment (Just t) = checkAST t


checkDeclareLocal :: Tree -> GenState ()
checkDeclareLocal node@(DeclarationNode name typ toAssign) = do
        ScopeCheck.checkIfUsedInScope node
        _ <- SymTab.addVariable name typ
        _ <- SymTab.stackPointerValue
        case toAssign of
             Just val -> checkAST val
             Nothing  -> pure ()
checkDeclareLocal tree = throwError $ ScopeError (UnexpectedNode tree)


checkDefineGlobal :: Tree -> GenState ()
checkDefineGlobal node@(AssignmentNode name _ _) = do
        ScopeCheck.checkIfDefined node
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        checkPrevDecGlob label node
checkDefineGlobal tree = throwError $ ScopeError (UnexpectedNode tree)


checkPrevDecGlob :: Maybe String -> Tree -> GenState ()
checkPrevDecGlob Nothing node = throwError $ ScopeError (UndeclaredNode node)
checkPrevDecGlob (Just _) (AssignmentNode _ node@(ConstantNode _) _)  = checkAST node
checkPrevDecGlob (Just _) (AssignmentNode _ node@(AddressOfNode _) _) = checkAST node
checkPrevDecGlob _ (AssignmentNode _ valNode _) =
        throwError $ ScopeError (UnexpectedNode valNode)
checkPrevDecGlob _ tree = throwError $ ScopeError (UnexpectedNode tree)


checkAssignLocal :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocal _ valTree Assignment = checkAST valTree
checkAssignLocal varTree valTree (BinaryOp binOp) =
        checkAST (BinaryNode varTree valTree binOp)
checkAssignLocal _ _ (UnaryOp a) =
        throwError $ GeneratorError (OperatorError (UnaryOp a))
