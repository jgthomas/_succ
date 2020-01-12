
module Checker (check) where


import           Control.Monad (unless, when)
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

checkAST node@(FunctionNode _ _ _ Nothing) = checkDecFunc node
checkAST node@(FunctionNode _ name _ (Just stmts)) = do
        Valid.checkIfFuncDefined node
        checkDecFunc node
        SymTab.initFunction name
        mapM_ checkAST stmts
        SymTab.closeFunction
        SymTab.defineFunction name

checkAST (ParamNode typ (VarNode name)) = SymTab.addParameter name typ
checkAST node@ParamNode{} = throwError $ SyntaxError (Unexpected node)

checkAST node@(FuncCallNode name argList) = do
        paramCount <- SymTab.decParamCount name
        Valid.checkArguments paramCount node
        TypeCheck.typesMatch name argList
        Valid.validateCall node
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
                     checkAST e
                     _ <- SymTab.labelNum
                     pure ()

checkAST (PointerNode varName typ Nothing) =
        checkAST (DeclarationNode varName typ Nothing)
checkAST node@(PointerNode varName typ (Just a)) = do
        checkAST (DeclarationNode varName typ Nothing)
        checkAST a
        _ <- Valid.checkVariableExists node
        pure ()

checkAST node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDecGlobal node
             Local  -> checkDecLocal node

checkAST AssignmentNode{} = pure ()

checkAST AssignDereferenceNode{} = pure ()

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

checkAST BinaryNode{} = pure ()

checkAST UnaryNode{} = pure ()

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


checkDecFunc :: Tree -> GenState ()
checkDecFunc node@(FunctionNode _ funcName _ _) = do
        Valid.checkIfVariable node
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> checkNewFuncDec node
             Just ns -> checkRepeatFuncDec ns node
checkDecFunc tree = throwError $ SyntaxError (Unexpected tree)


checkNewFuncDec :: Tree -> GenState ()
checkNewFuncDec (FunctionNode typ funcName paramList _) = do
        SymTab.declareFunction typ funcName (length paramList)
        checkParams funcName paramList
checkNewFuncDec tree = throwError $ SyntaxError (Unexpected tree)


checkRepeatFuncDec :: Int -> Tree -> GenState ()
checkRepeatFuncDec count node@(FunctionNode typ funcName paramList _) = do
        Valid.checkCountsMatch count node
        TypeCheck.typesMatch funcName paramList
        TypeCheck.funcDeclaration funcName typ
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              checkParams funcName paramList
checkRepeatFuncDec _ tree = throwError $ SyntaxError (Unexpected tree)


checkParams :: String -> [Tree] -> GenState ()
checkParams name params = do
        SymTab.initFunction name
        mapM_ checkAST params
        SymTab.closeFunction


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
