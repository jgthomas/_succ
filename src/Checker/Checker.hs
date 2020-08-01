{-|
Module       : Checker
Description  : Check AST for errors

Checks the AST for scope, type and other errors
-}
module Checker.Checker (check) where


import           Control.Monad      (unless)

import qualified Checker.LogicCheck as LogicCheck
import qualified Checker.ScopeCheck as ScopeCheck
import qualified Checker.TypeCheck  as TypeCheck
import           State.GenState     (GenState, runGenState, startState)
import qualified State.SymTab       as SymTab
import           Types.AST          (ArrayNode (..), Tree (..))
import           Types.Error        (CompilerError)
import           Types.Operator     (Operator (..), UnaryOp (..))
import           Types.Variables    (Scope (..))


-- | Check an AST for errors
check :: Tree -> Either CompilerError Tree
check ast = runGenState checker ast startState


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
        checkAST stmts
        SymTab.closeFunction
        SymTab.defineFunction name

checkAST (ParamNode typ (VarNode name _) _) =
        SymTab.addParameter name typ

checkAST node@ParamNode{} =
        LogicCheck.validateNode node

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
        checkAST varNode
        checkAST value
        TypeCheck.assignment node
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDefineGlobal node
             Local  -> checkAssignLocal varNode value op

checkAST node@(AssignDereferenceNode derefNode value op _) = do
        ScopeCheck.variableExists derefNode
        TypeCheck.assignment node
        checkAssignLocal derefNode value op

checkAST (ExprStmtNode expression _) = checkAST expression

checkAST node@(ContinueNode _) = ScopeCheck.checkGotoJump node

checkAST node@(BreakNode _) = ScopeCheck.checkGotoJump node

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

checkAST node@(UnaryNode varNode@VarNode{} _ _) = do
        LogicCheck.checkUnaryLogic node
        checkAST varNode
        ScopeCheck.variableExists varNode

checkAST node@(UnaryNode tree (Unary _) _) = do
        LogicCheck.checkUnaryLogic node
        checkAST tree

checkAST node@UnaryNode{} = LogicCheck.checkUnaryLogic node

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
        ScopeCheck.validateFuncDeclaration node
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> checkNewFuncDec node
             Just ns -> checkRepeatFuncDec ns node
checkFuncDec node = ScopeCheck.validateFuncDeclaration node


checkNewFuncDec :: Tree -> GenState ()
checkNewFuncDec (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        checkParams funcName paramList
checkNewFuncDec node = ScopeCheck.validateFuncDeclaration node


checkRepeatFuncDec :: Int -> Tree -> GenState ()
checkRepeatFuncDec count node@(FunctionNode typ funcName paramList _ _) = do
        ScopeCheck.checkParameters count node
        TypeCheck.typesMatch node
        TypeCheck.funcDeclaration node
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $ do
            SymTab.delFuncState funcName
            checkParams funcName paramList
checkRepeatFuncDec _ node = ScopeCheck.validateFuncDeclaration node


checkParams :: String -> [Tree] -> GenState ()
checkParams name params = do
        SymTab.initFunction name
        mapM_ checkAST params
        SymTab.closeFunction


checkDeclareGlobal :: Tree -> GenState ()
checkDeclareGlobal node@(DeclarationNode (VarNode name _) typ toAssign _) = do
        ScopeCheck.validateGlobalDeclaration node
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> do
                     checkAssignment toAssign
                     TypeCheck.globalDeclaration node
             Nothing -> do
                     globLab <- SymTab.mkGlobLabel name
                     SymTab.declareGlobal name typ globLab
                     checkAssignment toAssign
checkDeclareGlobal node = ScopeCheck.validateGlobalDeclaration node


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
checkDeclareLocal node = ScopeCheck.checkIfUsedInScope node


checkDefineGlobal :: Tree -> GenState ()
checkDefineGlobal node@(AssignmentNode (VarNode name _) _ _ _) = do
        ScopeCheck.checkIfDefined node
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        checkPrevDecGlob label node
checkDefineGlobal node = ScopeCheck.checkIfDefined node


checkPrevDecGlob :: Maybe String -> Tree -> GenState ()
checkPrevDecGlob (Just _) (AssignmentNode _ node@ConstantNode{} _ _)  = checkAST node
checkPrevDecGlob (Just _) (AssignmentNode _ node@AddressOfNode{} _ _) = checkAST node
checkPrevDecGlob name node = ScopeCheck.validatePrevDecGlobal name node


checkAssignLocal :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocal varTree valTree Assignment = do
        LogicCheck.checkAssignLocalLogic varTree valTree Assignment
        checkAST valTree
checkAssignLocal varTree@(DereferenceNode _ dat) valTree op@(BinaryOp binOp) = do
        LogicCheck.checkAssignLocalLogic varTree valTree op
        checkAST (BinaryNode varTree valTree binOp dat)
checkAssignLocal varTree@(VarNode _ dat) valTree op@(BinaryOp binOp) = do
        LogicCheck.checkAssignLocalLogic varTree valTree op
        checkAST (BinaryNode varTree valTree binOp dat)
checkAssignLocal varTree valTree op =
        LogicCheck.checkAssignLocalLogic varTree valTree op
