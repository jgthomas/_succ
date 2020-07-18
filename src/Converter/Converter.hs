
module Converter.Converter (convert) where


import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe)

import           State.GenState       (GenState, runGenState, throwError)
import qualified State.GenState       as GenState (getState, startState)
import           State.SymTab         (SymTab)
import qualified State.SymTab         as SymTab
import           Types.AssemblySchema
import           Types.AST            (Tree (..))
import           Types.Error          (CompilerError (FatalError),
                                       FatalError (GeneratorBug))
import           Types.Variables      (Scope (..), VarLookup (..))


convert :: Tree -> Either CompilerError (AssemblySchema, SymTab)
convert ast = runGenState convertWithState ast GenState.startState


convertWithState :: Tree -> GenState (AssemblySchema, SymTab)
convertWithState ast = do
        schema <- convertToSchema ast
        symTab <- GenState.getState
        pure (schema, symTab)


convertToSchema :: Tree -> GenState AssemblySchema

convertToSchema (ProgramNode trees) = do
        schemas <- mapM convertToSchema trees
        pure (ProgramSchema schemas)

convertToSchema funcNode@(FunctionNode _ _ _ Nothing _) = do
        declareFunction funcNode
        pure SkipSchema
convertToSchema funcNode@(FunctionNode _ name _ (Just body) _) = do
        declareFunction funcNode
        SymTab.initFunction name
        bodySchema <- convertToSchema body
        SymTab.closeFunction
        SymTab.defineFunction name
        pure (FunctionSchema name bodySchema)

convertToSchema (ParamNode typ (VarNode name _) _) = do
        SymTab.addParameter name typ
        pure SkipSchema
convertToSchema node@ParamNode{} =
        throwError $ FatalError (GeneratorBug node)

convertToSchema (ArgNode arg _) = convertToSchema arg

convertToSchema (CompoundStmtNode statements _) = do
        SymTab.initScope
        statementsSchema <- mapM convertToSchema statements
        SymTab.closeScope
        pure (StatementSchema $ CompoundStatementSchema statementsSchema)

convertToSchema (ForLoopNode ini test iter body _) = do
        SymTab.initScope
        passLabel <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue contLabel
        iniSchema  <- convertToSchema ini
        testSchema <- getExpressionSchema <$> convertToSchema test
        iterSchema <- getExpressionSchema <$> convertToSchema iter
        bodySchema <- getStatementSchema <$> convertToSchema body
        SymTab.closeScope
        pure (StatementSchema
              (ForSchema
               iniSchema
               testSchema
               iterSchema
               bodySchema
               (LocalLabel passLabel)
               (LocalLabel failLabel)
               (LocalLabel contLabel)
              )
             )

convertToSchema (WhileNode test body _) = do
        loopLabel  <- SymTab.labelNum
        testLabel  <- SymTab.labelNum
        testSchema <- getExpressionSchema <$> convertToSchema test
        bodySchema <- getStatementSchema <$> convertToSchema body
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        pure (StatementSchema
              (WhileSchema
               testSchema
               bodySchema
               (LocalLabel loopLabel)
               (LocalLabel testLabel)
              )
             )

convertToSchema (DoWhileNode body test _) = do
        loopLabel  <- SymTab.labelNum
        contLabel  <- SymTab.labelNum
        testLabel  <- SymTab.labelNum
        bodySchema <- getStatementSchema <$> convertToSchema body
        testSchema <- getExpressionSchema <$> convertToSchema test
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        pure (StatementSchema
              (DoWhileSchema
               bodySchema
               testSchema
               (LocalLabel loopLabel)
               (LocalLabel contLabel)
               (LocalLabel testLabel)
              )
             )

convertToSchema (IfNode test body possElse _) = do
        ifLabel    <- LocalLabel <$> SymTab.labelNum
        elseLabel  <- LocalLabel <$> SymTab.labelNum
        testSchema <- getExpressionSchema <$> convertToSchema test
        bodySchema <- getStatementSchema <$> convertToSchema body
        elseSchema <- processElse possElse
        pure (StatementSchema
              (IfSchema
               testSchema
               bodySchema
               elseSchema
               ifLabel
               elseLabel
              )
             )

convertToSchema node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Local  -> declareLocal node
             Global -> declareGlobal node

convertToSchema node@AssignmentNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Local  -> defineLocal node
             Global -> defineGlobal node

convertToSchema (ExprStmtNode exprStatement _) = convertToSchema exprStatement

convertToSchema ContinueNode{} = do
        contineLabel <- LocalLabel . fromMaybe (-1) <$> SymTab.getContinue
        pure (StatementSchema $ ContinueSchema contineLabel)

convertToSchema BreakNode{} = do
        breakLabel <- LocalLabel . fromMaybe (-1) <$> SymTab.getBreak
        pure (StatementSchema $ BreakSchema breakLabel)

convertToSchema (ReturnNode val _) = do
        value <- getExpressionSchema <$> convertToSchema val
        pure (StatementSchema $ ReturnSchema value)

convertToSchema (TernaryNode test true false _) = do
        testSchema  <- getExpressionSchema <$> convertToSchema test
        trueSchema  <- getExpressionSchema <$> convertToSchema true
        falseSchema <- getExpressionSchema <$> convertToSchema false
        trueLabel   <- LocalLabel <$> SymTab.labelNum
        pure (ExpressionSchema
              (TernarySchema
               testSchema
               trueSchema
               falseSchema
               trueLabel
              )
             )

convertToSchema (UnaryNode val unOp _) = do
        value <- getExpressionSchema <$> convertToSchema val
        pure (ExpressionSchema $ UnarySchema value unOp)

convertToSchema (ConstantNode n _) = pure (ExpressionSchema $ LiteralSchema n)

convertToSchema node@(VarNode name _) = do
        varType <- SymTab.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (GeneratorBug node)
             VarType typ -> pure (ExpressionSchema $ VariableSchema typ)

convertToSchema NullExprNode{} = pure SkipSchema

convertToSchema tree = throwError $ FatalError (GeneratorBug tree)


getExpressionSchema :: AssemblySchema -> ExpressionSchema
getExpressionSchema (ExpressionSchema schema) = schema
getExpressionSchema _                         = undefined


getStatementSchema :: AssemblySchema -> StatementSchema
getStatementSchema (StatementSchema schema) = schema
getStatementSchema _                        = undefined



-- Function

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode _ funcName _ _ _) = do
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> declareNewFunction node
             Just _  -> declareRepeatFunction node
declareFunction tree = throwError $ FatalError (GeneratorBug tree)


declareNewFunction :: Tree -> GenState ()
declareNewFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        processParameters funcName paramList
declareNewFunction tree = throwError $ FatalError (GeneratorBug tree)


declareRepeatFunction :: Tree -> GenState ()
declareRepeatFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ FatalError (GeneratorBug tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ convertToSchema params
        SymTab.closeFunction


-- Variables Global

declareGlobal :: Tree -> GenState AssemblySchema
declareGlobal (DeclarationNode varNode@(VarNode name _) typ assignNode _) = do
        currLabel    <- SymTab.globalLabel name
        case currLabel of
             Just _  -> pure SkipSchema
             Nothing -> do
                     globLab <- SymTab.mkGlobLabel name
                     SymTab.declareGlobal name typ globLab
                     currScope    <- SymTab.getScope
                     varSchema    <- convertToSchema varNode
                     assignSchema <- processAssignment assignNode
                     pure (DeclarationSchema varSchema assignSchema currScope)
declareGlobal tree = throwError $ FatalError (GeneratorBug tree)


defineGlobal :: Tree -> GenState AssemblySchema
defineGlobal (AssignmentNode varNode@(VarNode name _) valNode _ _) = do
        SymTab.defineGlobal name
        currScope <- SymTab.getScope
        varSchema <- getExpressionSchema <$> convertToSchema varNode
        valSchema <- getExpressionSchema <$> convertToSchema valNode
        pure (StatementSchema $ AssignmentSchema varSchema valSchema currScope)
defineGlobal tree = throwError $ FatalError (GeneratorBug tree)



-- Variables Local

declareLocal :: Tree -> GenState AssemblySchema
declareLocal (DeclarationNode varNode@(VarNode name _) typ value _) = do
        _ <- SymTab.addVariable name typ
        currScope <- SymTab.getScope
        varSchema <- convertToSchema varNode
        valSchema <- processAssignment value
        pure (DeclarationSchema varSchema valSchema currScope)
declareLocal tree = throwError $ FatalError (GeneratorBug tree)


defineLocal :: Tree -> GenState AssemblySchema
defineLocal (AssignmentNode varNode value _ _) = do
        currScope <- SymTab.getScope
        varSchema <- getExpressionSchema <$> convertToSchema varNode
        valSchema <- getExpressionSchema <$> convertToSchema value
        pure (StatementSchema $ AssignmentSchema varSchema valSchema currScope)
defineLocal tree = throwError $ FatalError (GeneratorBug tree)



-- Variables Shared

processAssignment :: Maybe Tree -> GenState AssemblySchema
processAssignment Nothing           = pure SkipSchema
processAssignment (Just assignNode) = convertToSchema assignNode


-- Statements

processElse :: Maybe Tree -> GenState AssemblySchema
processElse Nothing  = pure SkipSchema
processElse (Just e) = convertToSchema e
