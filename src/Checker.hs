
module Checker (check) where


import           Control.Monad (when)
import           Data.Maybe    (isNothing)

import           AST           (Tree (..))
import           Error         (CompilerError (SyntaxError), SyntaxError (..))
import           GenState      (GenState, runGenState, throwError)
import qualified GenState      (startState)
import           GenTokens     (Scope (..))
import qualified SymTab
import qualified TypeCheck
import qualified Validate      as Valid


check :: Tree -> Either CompilerError Tree
check ast = runGenState checker ast GenState.startState


checker :: Tree -> GenState Tree
checker ast = do
        checkAST ast
        pure ast


checkAST :: Tree -> GenState ()

checkAST (ProgramNode topLevelItems) = mapM_ checkAST topLevelItems

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
                     checkAST e
                     _ <- SymTab.labelNum
                     pure ()

checkAST node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDecGlobal node
             Local  -> checkDecLocal node

checkAST (ExprStmtNode expression) = checkAST expression

checkAST ContinueNode = do
        continueLabel <- SymTab.getContinue
        when (isNothing continueLabel) $
            throwError $ SyntaxError (Unexpected ContinueNode)

checkAST BreakNode = do
        breakLabel <- SymTab.getContinue
        when (isNothing breakLabel) $
            throwError $ SyntaxError (Unexpected BreakNode)

checkAST (ReturnNode tree) = do
        TypeCheck.funcReturn tree
        checkAST tree

checkAST (TernaryNode cond pass fails) = do
        checkAST cond
        checkAST pass
        checkAST fails
        _ <- SymTab.labelNum
        _ <- SymTab.labelNum
        pure ()

checkAST node@(VarNode _) = do
        _ <- Valid.checkVariableExists node
        pure ()

checkAST node@(AddressOfNode _) = do
        _ <- Valid.checkVariableExists node
        pure ()

checkAST node@(DereferenceNode _) = do
        _ <- Valid.checkVariableExists node
        pure ()

checkAST NullExprNode = pure ()

checkAST (ConstantNode _) = pure ()

checkAST _ = pure ()


checkDecGlobal :: Tree -> GenState ()
checkDecGlobal node@(DeclarationNode name typ toAssign) = do
        Valid.checkIfFunction node
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> TypeCheck.globalDeclaration name typ
             Nothing -> do
                     globLab <- Valid.mkGlobLabel name <$> SymTab.labelNum
                     SymTab.declareGlobal name typ globLab
                     checkAssignment toAssign
checkDecGlobal tree = throwError $ SyntaxError (Unexpected tree)


checkAssignment :: Maybe Tree -> GenState ()
checkAssignment Nothing  = pure ()
checkAssignment (Just t) = checkAST t


checkDecLocal :: Tree -> GenState ()
checkDecLocal node@(DeclarationNode name typ toAssign) = do
        Valid.checkIfUsedInScope node
        _ <- SymTab.addVariable name typ
        _ <- SymTab.stackPointerValue
        case toAssign of
             Just val -> checkAST val
             Nothing  -> pure ()
checkDecLocal tree = throwError $ SyntaxError (Unexpected tree)
