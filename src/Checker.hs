
module Checker (check) where


import           Control.Monad (unless, when)
import           Data.Maybe    (isNothing)

import           AST           (Tree (..))
import           Error         (CompilerError (GeneratorError, SyntaxError),
                                GeneratorError (..), SyntaxError (..))
import           GenState      (GenState, runGenState, throwError)
import qualified GenState      (startState)
import           GenTokens     (Scope (..))
import           Operator      (Operator (..), UnaryOp (..))
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
        Valid.variableExists node

checkAST node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDecGlobal node
             Local  -> checkDecLocal node

checkAST node@(AssignmentNode varName value op) = do
        TypeCheck.assignment varName value
        currScope <- SymTab.getScope
        case currScope of
             Global -> checkDefineGlobal node
             Local  -> do
                     checkAssignLocal (VarNode varName) value op
                     (offset, _, globLab) <- Valid.checkVariableExists node
                     case (offset, globLab) of
                          (Just _, _) -> do
                                  _ <- SymTab.stackPointerValue
                                  pure ()
                          (_, Just _) -> pure ()
                          _ -> throwError $ SyntaxError (Undeclared node)

checkAST node@(AssignDereferenceNode varName value op) = do
        TypeCheck.assignment varName value
        checkAssignLocal (DereferenceNode varName) value op
        Valid.variableExists node

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
        checkAST tree
        TypeCheck.funcReturn tree

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
        Valid.variableExists node
checkAST (UnaryNode _ unOp@(PreOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
checkAST (UnaryNode _ unOp@(PostOpUnary _)) =
        throwError $ GeneratorError (OperatorError (UnaryOp unOp))
checkAST (UnaryNode tree (Unary _)) = checkAST tree

checkAST node@(VarNode _) = Valid.variableExists node

checkAST node@(AddressOfNode _) = Valid.variableExists node

checkAST node@(DereferenceNode _) = Valid.variableExists node

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


checkDefineGlobal :: Tree -> GenState ()
checkDefineGlobal node@(AssignmentNode name _ _) = do
        Valid.checkIfDefined node
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        checkPrevDecGlob label node
checkDefineGlobal tree = throwError $ SyntaxError (Unexpected tree)


checkPrevDecGlob :: Maybe String -> Tree -> GenState ()
checkPrevDecGlob Nothing node = throwError $ SyntaxError (Undeclared node)
checkPrevDecGlob (Just _) (AssignmentNode _ node@(ConstantNode _) _)  = checkAST node
checkPrevDecGlob (Just _) (AssignmentNode _ node@(AddressOfNode _) _) = checkAST node
checkPrevDecGlob _ (AssignmentNode _ valNode _) =
        throwError $ SyntaxError (Unexpected valNode)
checkPrevDecGlob _ tree = throwError $ SyntaxError (Unexpected tree)


checkAssignLocal :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocal _ valTree Assignment = checkAST valTree
checkAssignLocal varTree valTree (BinaryOp binOp) =
        checkAST (BinaryNode varTree valTree binOp)
checkAssignLocal _ _ (UnaryOp a) =
        throwError $ GeneratorError (OperatorError (UnaryOp a))
