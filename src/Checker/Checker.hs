{-|
Module       : Checker
Description  : Check AST for errors

Checks the AST for scope, type and other errors
-}
module Checker.Checker (check) where


import           Control.Monad      (unless, when)
import           Data.Maybe         (isNothing)

import qualified Checker.ScopeCheck as ScopeCheck
import qualified Checker.TypeCheck  as TypeCheck
import           Error.Error        (CheckerError (..),
                                     CompilerError (CheckerError, ScopeError),
                                     ScopeError (..))
import           GenState           (GenState, runGenState, throwError)
import qualified GenState           (startState)
import           GenTokens          (Scope (..))
import qualified SymTab
import           Types.AST          (ArrayNode (..), Tree (..))
import           Types.Operator     (Operator (..), UnaryOp (..))


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

checkAST (ParamNode typ (VarNode name _) _) =
        SymTab.addParameter name typ
checkAST node@ParamNode{} =
        throwError $ ScopeError (UnexpectedNode node)

checkAST node@(FuncCallNode name argList _) = do
        paramCount <- SymTab.decParamCount name
        ScopeCheck.checkArguments paramCount node
        TypeCheck.typesMatch node
        ScopeCheck.validateCall node
        mapM_ checkAST argList

checkAST (ArgNode arg _) = checkAST arg

checkAST (CompoundStmtNode blockItems _) = do
        SymTab.initScope
        mapM_ checkAST blockItems
        SymTab.closeScope

checkAST (ForLoopNode ini test iter block _) = do
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

checkAST (WhileNode test whileBlock _) = do
        loopLabel <- SymTab.labelNum
        checkAST test
        testLabel <- SymTab.labelNum
        checkAST whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel

checkAST (DoWhileNode block test _) = do
        _         <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        checkAST block
        checkAST test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel

checkAST (IfNode test action possElse _) = do
        checkAST test
        checkAST action
        _ <- SymTab.labelNum
        case possElse of
             Nothing -> pure ()
             Just e  -> do
                     _ <- SymTab.labelNum
                     checkAST e

checkAST (PointerNode varNode typ Nothing dat) =
        checkAST (DeclarationNode varNode typ Nothing dat)
checkAST (PointerNode varNode typ (Just a) dat) = do
        checkAST (DeclarationNode varNode typ Nothing dat)
        ScopeCheck.variableExists varNode
        checkAST a

checkAST node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDeclareGlobal node
             Local  -> checkDeclareLocal node

checkAST node@(AssignmentNode varNode value op _) = do
        TypeCheck.assignment node
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDefineGlobal node
             Local  -> do
                     ScopeCheck.variableExists varNode
                     checkAssignLocal varNode value op

checkAST node@(AssignDereferenceNode derefNode value op _) = do
        ScopeCheck.variableExists derefNode
        TypeCheck.assignment node
        checkAssignLocal derefNode value op

checkAST (ExprStmtNode expression _) = checkAST expression

checkAST node@(ContinueNode _) = do
        continueLabel <- SymTab.getContinue
        when (isNothing continueLabel) $
            throwError $ ScopeError (UnexpectedNode node)

checkAST node@(BreakNode _) = do
        breakLabel <- SymTab.getBreak
        when (isNothing breakLabel) $
            throwError $ ScopeError (UnexpectedNode node)

checkAST node@(ReturnNode tree _) = do
        checkAST tree
        TypeCheck.funcReturn node tree

checkAST (TernaryNode cond pass fails _) = do
        _ <- SymTab.labelNum
        _ <- SymTab.labelNum
        checkAST cond
        checkAST pass
        checkAST fails

checkAST (BinaryNode lft rgt _ _) = do
        _ <- SymTab.labelNum
        _ <- SymTab.labelNum
        checkAST lft
        checkAST rgt

checkAST (UnaryNode node@VarNode{} _ _) = do
        checkAST node
        ScopeCheck.variableExists node
checkAST node@(UnaryNode _ unOp@(PreOpUnary _) _) =
        throwError $ CheckerError (OperatorError (UnaryOp unOp) node)
checkAST node@(UnaryNode _ unOp@(PostOpUnary _) _) =
        throwError $ CheckerError (OperatorError (UnaryOp unOp) node)
checkAST (UnaryNode tree (Unary _) _) = checkAST tree

checkAST node@VarNode{} = ScopeCheck.variableExists node

checkAST node@AddressOfNode{} = ScopeCheck.variableExists node

checkAST node@DereferenceNode{} = ScopeCheck.variableExists node

checkAST NullExprNode{} = pure ()

checkAST ConstantNode{} = pure ()

checkAST (ArrayNode arrayNode) = checkArrayAST arrayNode


checkArrayAST :: ArrayNode -> GenState ()

checkArrayAST (ArrayDeclareNode len var typ assign dat) = do
        checkDeclareLocal (DeclarationNode var typ assign dat)
        SymTab.incrementOffsetByN (len - 1)

checkArrayAST (ArrayItemsNode varNode itemList _) = do
        checkAST varNode
        mapM_ checkAST itemList

checkArrayAST (ArraySingleItemNode item _) = checkAST item

checkArrayAST (ArrayItemAccess _ varNode _) = checkAST varNode

checkArrayAST (ArrayItemAssign _ varNode _) = checkAST varNode

checkArrayAST (ArrayAssignPosNode varNode valNode _ _) = do
        checkAST varNode
        checkAST valNode


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
checkDeclareGlobal node@(DeclarationNode (VarNode name _) typ toAssign _) = do
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
checkDeclareLocal node@(DeclarationNode (VarNode name _) typ toAssign _) = do
        ScopeCheck.checkIfUsedInScope node
        _ <- SymTab.addVariable name typ
        _ <- SymTab.stackPointerValue
        case toAssign of
             Just val -> checkAST val
             Nothing  -> pure ()
checkDeclareLocal tree = throwError $ ScopeError (UnexpectedNode tree)


checkDefineGlobal :: Tree -> GenState ()
checkDefineGlobal node@(AssignmentNode (VarNode name _) _ _ _) = do
        ScopeCheck.checkIfDefined node
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        checkPrevDecGlob label node
checkDefineGlobal tree = throwError $ ScopeError (UnexpectedNode tree)


checkPrevDecGlob :: Maybe String -> Tree -> GenState ()
checkPrevDecGlob Nothing node = throwError $ ScopeError (UndeclaredNode node)
checkPrevDecGlob (Just _) (AssignmentNode _ node@ConstantNode{} _ _)  = checkAST node
checkPrevDecGlob (Just _) (AssignmentNode _ node@AddressOfNode{} _ _) = checkAST node
checkPrevDecGlob _ (AssignmentNode _ valNode _ _) =
        throwError $ ScopeError (UnexpectedNode valNode)
checkPrevDecGlob _ tree = throwError $ ScopeError (UnexpectedNode tree)


checkAssignLocal :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocal _ valTree Assignment = checkAST valTree
checkAssignLocal varTree@(DereferenceNode _ dat) valTree (BinaryOp binOp) =
        checkAST (BinaryNode varTree valTree binOp dat)
checkAssignLocal varTree@(VarNode _ dat) valTree (BinaryOp binOp) =
        checkAST (BinaryNode varTree valTree binOp dat)
checkAssignLocal node _ op@(UnaryOp _) =
        throwError $ CheckerError (OperatorError op node)
checkAssignLocal node _ _ = throwError $ CheckerError (InvalidNode node)
