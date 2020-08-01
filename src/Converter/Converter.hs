{-|
Module       : Converter
Description  : Convert syntax tree to assembly schema

Converts an abstract syntax tree into an assembly schema
-}
module Converter.Converter (convert) where


import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe)

import           State.GenState       (GenState, runGenState, throwError)
import qualified State.GenState       as GenState (getState, startState)
import qualified State.GlobalState    as GlobalState
import           State.SymTab         (SymTab)
import qualified State.SymTab         as SymTab
import           Types.AssemblySchema
import           Types.AST            (ArrayNode (..), Tree (..))
import           Types.Error          (CompilerError (FatalError),
                                       FatalError (ConverterBug))
import           Types.Operator
import           Types.Type
import           Types.Variables


-- | Builds an assembly schema
convert :: Tree -> Either CompilerError (AssemblySchema, SymTab)
convert ast = runGenState convertWithState ast GenState.startState


convertWithState :: Tree -> GenState (AssemblySchema, SymTab)
convertWithState ast = do
        schema <- convertToSchema ast
        symTab <- GenState.getState
        pure (schema, symTab)


convertToSchema :: Tree -> GenState AssemblySchema

convertToSchema (ProgramNode trees) = do
        schemas      <- mapM convertToSchema trees
        undefSchemas <- map buildUndefinedSchema <$> GlobalState.getUndefinedVarData
        pure (ProgramSchema $ undefSchemas ++ schemas)

convertToSchema funcNode@(FunctionNode _ _ _ Nothing _) = do
        declareFunction funcNode
        pure SkipSchema

convertToSchema funcNode@(FunctionNode _ name _ (Just body) _) = do
        declareFunction funcNode
        SymTab.initFunction name
        bodySchema <- checkReturn name <$> convertToSchema body
        SymTab.closeFunction
        GlobalState.defineFunction name
        pure (FunctionSchema name bodySchema)

convertToSchema (ParamNode typ (VarNode name _) _) = do
        SymTab.addParameter name typ
        pure SkipSchema

convertToSchema (FuncCallNode name argList _) = do
        schemas <- mapM convertToSchema argList
        let argSchemas = map getExpressionSchema schemas
        pure (ExpressionSchema $ FunctionCallSchema name argSchemas)

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
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        testSchema <- getExpressionSchema <$> convertToSchema test
        bodySchema <- getStatementSchema <$> convertToSchema body
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
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        bodySchema <- getStatementSchema <$> convertToSchema body
        testSchema <- getExpressionSchema <$> convertToSchema test
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
        elseSchema <- processPossibleNode possElse
        pure (StatementSchema
              (IfSchema
               testSchema
               bodySchema
               elseSchema
               ifLabel
               elseLabel
              )
             )

convertToSchema (PointerNode varNode typ value dat) =
        convertToSchema (DeclarationNode varNode typ value dat)

convertToSchema node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Local  -> declareLocal node
             Global -> declareGlobal node

convertToSchema node@AssignmentNode{} = buildAssignmentSchema node

convertToSchema (AssignDereferenceNode varNode valNode operator dat) =
        convertToSchema (AssignmentNode varNode valNode operator dat)

convertToSchema (ExprStmtNode exprStatement _) = convertToSchema exprStatement

convertToSchema ContinueNode{} = do
        contineLabel <- LocalLabel . fromMaybe (-1) <$> SymTab.getContinue
        pure (StatementSchema $ ContinueSchema contineLabel)

convertToSchema BreakNode{} = do
        breakLabel <- LocalLabel . fromMaybe (-1) <$> SymTab.getBreak
        pure (StatementSchema $ BreakSchema breakLabel)

convertToSchema (ReturnNode val _) = do
        valueSchema <- getExpressionSchema <$> convertToSchema val
        pure (StatementSchema $ ReturnSchema valueSchema)

convertToSchema (TernaryNode test true false _) = do
        testSchema  <- getExpressionSchema <$> convertToSchema test
        trueSchema  <- getExpressionSchema <$> convertToSchema true
        falseSchema <- getExpressionSchema <$> convertToSchema false
        trueLabel   <- LocalLabel <$> SymTab.labelNum
        falseLabel  <- LocalLabel <$> SymTab.labelNum
        pure (ExpressionSchema
              (TernarySchema
               testSchema
               trueSchema
               falseSchema
               trueLabel
               falseLabel
              )
             )

convertToSchema (BinaryNode leftNode rightNode operator _) = do
        trueLabel   <- LocalLabel <$> SymTab.labelNum
        falseLabel  <- LocalLabel <$> SymTab.labelNum
        leftSchema  <- getExpressionSchema <$> convertToSchema leftNode
        rightSchema <- getExpressionSchema <$> convertToSchema rightNode
        pure (ExpressionSchema
              (BinarySchema
               leftSchema
               rightSchema
               operator
               trueLabel
               falseLabel
              )
             )

convertToSchema (UnaryNode val unOp _) = do
        value <- getExpressionSchema <$> convertToSchema val
        pure (ExpressionSchema $ UnarySchema value unOp)

convertToSchema (ConstantNode n _) = pure (ExpressionSchema $ LiteralSchema n)

convertToSchema node@(VarNode name _) = do
        varType <- SymTab.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType typ -> pure (ExpressionSchema $ VariableSchema typ)

convertToSchema node@(DereferenceNode name _) = do
        varType <- SymTab.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType var -> pure (ExpressionSchema
                                  (DereferenceSchema
                                   (VariableSchema var)
                                  )
                                 )

convertToSchema node@(AddressOfNode name _) = do
        varType <- SymTab.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType var -> pure (ExpressionSchema
                                  (AddressOfSchema
                                   (VariableSchema var)
                                  )
                                 )

convertToSchema NullExprNode{} = pure SkipSchema

convertToSchema (ArrayNode arrayNode) = convertToSchemaArray arrayNode

convertToSchema node = throwError $ FatalError (ConverterBug node)


convertToSchemaArray :: ArrayNode -> GenState AssemblySchema

convertToSchemaArray (ArrayDeclareNode len var typ Nothing dat) = do
        declareSchema <- convertToSchema (DeclarationNode var typ Nothing dat)
        SymTab.incrementOffsetByN (len - 1)
        pure declareSchema

convertToSchemaArray  (ArrayDeclareNode _ var typ assign dat) =
        convertToSchema (DeclarationNode var typ assign dat)

convertToSchemaArray (ArrayItemsNode varNode items _) = processArrayItems varNode items

convertToSchemaArray (ArraySingleItemNode item _) = convertToSchema item

convertToSchemaArray (ArrayItemAccess pos varNode _) = getArrayIndexItem pos varNode

convertToSchemaArray node@(ArrayAssignPosNode (ArrayNode (ArrayItemAssign pos varNode iDat)) valNode op dat) =
        case op of
             Assignment     -> processArrayItem varNode (valNode, pos)
             UnaryOp _      -> throwError $ FatalError (ConverterBug $ ArrayNode node)
             BinaryOp binOp -> convertToSchema $ AssignmentNode
                                                 varNode
                                                 (BinaryNode varNode valNode binOp iDat)
                                                 Assignment
                                                 dat

convertToSchemaArray (ArrayItemAssign pos varNode _) = getArrayIndexItem pos varNode

convertToSchemaArray node = throwError $ FatalError (ConverterBug $ ArrayNode node)


-- Array

getArrayIndexItem :: Int -> Tree -> GenState AssemblySchema
getArrayIndexItem pos varNode@VarNode{} = do
        varSchema <- setSchemaOffset pos . getExpressionSchema <$> convertToSchema varNode
        pure (ExpressionSchema varSchema)
getArrayIndexItem _ node = throwError $ FatalError (ConverterBug node)


processArrayItems :: Tree -> [Tree] -> GenState AssemblySchema
processArrayItems varNode items = do
        arrayItemsSchema <- mapM (processArrayItem varNode) (zip items [0..])
        SymTab.incrementOffsetByN (length items - 1)
        adjust           <- SymTab.stackPointerValue
        pure (ExpressionSchema
              (ArrayItemsSchema
               adjust
               (map getStatementSchema arrayItemsSchema)
              )
             )


processArrayItem :: Tree -> (Tree, Int) -> GenState AssemblySchema
processArrayItem varNode (item, pos) = do
        currScope <- SymTab.getScope
        varSchema <- getExpressionSchema <$> getArrayIndexItem pos varNode
        valSchema <- getExpressionSchema <$> convertToSchema item
        pure (StatementSchema
              (AssignmentSchema
               varSchema
               valSchema
               currScope
              )
             )


setSchemaOffset :: Int -> ExpressionSchema -> ExpressionSchema
setSchemaOffset n (VariableSchema varType) =
        VariableSchema $ adjustVariable (Just n) Nothing varType
setSchemaOffset _ _ = undefined


-- Function

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode _ funcName _ _ _) = do
        prevParamCount <- GlobalState.decParamCount funcName
        case prevParamCount of
             Nothing -> declareNewFunction node
             Just _  -> declareRepeatFunction node
declareFunction tree = throwError $ FatalError (ConverterBug tree)


declareNewFunction :: Tree -> GenState ()
declareNewFunction (FunctionNode typ funcName paramList _ _) = do
        GlobalState.declareFunction typ funcName (length paramList)
        processParameters funcName paramList
declareNewFunction tree = throwError $ FatalError (ConverterBug tree)


declareRepeatFunction :: Tree -> GenState ()
declareRepeatFunction (FunctionNode typ funcName paramList _ _) = do
        GlobalState.declareFunction typ funcName (length paramList)
        defined <- GlobalState.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ FatalError (ConverterBug tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ convertToSchema params
        SymTab.closeFunction


checkReturn :: String -> AssemblySchema -> AssemblySchema
checkReturn "main" (StatementSchema (CompoundStatementSchema [])) =
        StatementSchema (CompoundStatementSchema $ addReturnZero [])
checkReturn "main" schema@(StatementSchema (CompoundStatementSchema bodySchemas)) =
        case last bodySchemas of
             (StatementSchema ReturnSchema{}) -> schema
             _ -> StatementSchema (CompoundStatementSchema $ addReturnZero bodySchemas)
checkReturn _ schema = schema


addReturnZero :: [AssemblySchema] -> [AssemblySchema]
addReturnZero bodySchema = bodySchema ++ [StatementSchema (ReturnSchema (LiteralSchema 0))]


-- Variables Global

declareGlobal :: Tree -> GenState AssemblySchema
declareGlobal (DeclarationNode (VarNode name _) typ Nothing _) = do
        globLab <- GlobalState.mkGlobLabel name
        GlobalState.declareGlobal name typ globLab
        pure SkipSchema
declareGlobal node@(DeclarationNode (VarNode name _) typ _ _) = do
        currLabel <- GlobalState.globalLabel name
        case currLabel of
             Just _  -> processGlobalAssignment node
             Nothing -> do
                     globLab <- GlobalState.mkGlobLabel name
                     GlobalState.declareGlobal name typ globLab
                     processGlobalAssignment node
declareGlobal tree = throwError $ FatalError (ConverterBug tree)


processGlobalAssignment :: Tree -> GenState AssemblySchema
processGlobalAssignment (DeclarationNode varNode typ (Just assignNode) _) = do
        currScope    <- SymTab.getScope
        varSchema    <- convertToSchema varNode
        assignSchema <- convertToSchema assignNode
        pure (DeclarationSchema varSchema assignSchema currScope typ)
processGlobalAssignment tree = throwError $ FatalError (ConverterBug tree)


defineGlobal :: Tree -> GenState AssemblySchema
defineGlobal (AssignmentNode varNode@(VarNode name _) valNode _ _) = do
        GlobalState.defineGlobal name
        currScope <- SymTab.getScope
        varSchema <- getExpressionSchema <$> convertToSchema varNode
        valSchema <- getExpressionSchema <$> convertToSchema valNode
        pure (StatementSchema $ AssignmentSchema varSchema valSchema currScope)
defineGlobal tree = throwError $ FatalError (ConverterBug tree)


buildUndefinedSchema :: (String, Type) -> AssemblySchema
buildUndefinedSchema (label, typ) =
        DeclarationSchema
        (ExpressionSchema $ VariableSchema $ GlobalVar label 0)
        SkipSchema
        Global
        typ


-- Variables Local

declareLocal :: Tree -> GenState AssemblySchema
declareLocal (DeclarationNode varNode@(VarNode name _) typ value _) = do
        _ <- SymTab.addVariable name typ
        currScope <- SymTab.getScope
        varSchema <- convertToSchema varNode
        valSchema <- processPossibleNode value
        pure (DeclarationSchema varSchema valSchema currScope typ)
declareLocal tree = throwError $ FatalError (ConverterBug tree)


defineLocal :: Tree -> GenState AssemblySchema
defineLocal (AssignmentNode varNode value _ _) = do
        currScope <- SymTab.getScope
        varSchema <- getExpressionSchema <$> convertToSchema varNode
        valSchema <- getExpressionSchema <$> convertToSchema value
        pure (StatementSchema $ AssignmentSchema varSchema valSchema currScope)
defineLocal tree = throwError $ FatalError (ConverterBug tree)



-- Shared

buildAssignmentSchema :: Tree -> GenState AssemblySchema
buildAssignmentSchema node@(AssignmentNode _ _ Assignment _) =
        buildBasicAssignment node
buildAssignmentSchema (AssignmentNode varNode valNode (BinaryOp binOp) dat ) =
        convertToSchema $ AssignmentNode
                          varNode
                          (BinaryNode varNode valNode binOp dat)
                          Assignment
                          dat
buildAssignmentSchema node = throwError $ FatalError (ConverterBug node)


buildBasicAssignment :: Tree -> GenState AssemblySchema
buildBasicAssignment node = do
        currScope <- SymTab.getScope
        case currScope of
             Local  -> defineLocal node
             Global -> defineGlobal node


getExpressionSchema :: AssemblySchema -> ExpressionSchema
getExpressionSchema (ExpressionSchema schema) = schema
getExpressionSchema (StatementSchema schema)  = ExpressionStatementSchema schema
getExpressionSchema SkipSchema                = NullExpressionSchema
getExpressionSchema _                         = undefined


getStatementSchema :: AssemblySchema -> StatementSchema
getStatementSchema (StatementSchema schema) = schema
getStatementSchema SkipSchema               = NullStatementSchema
getStatementSchema _                        = undefined


processPossibleNode :: Maybe Tree -> GenState AssemblySchema
processPossibleNode Nothing     = pure SkipSchema
processPossibleNode (Just node) = convertToSchema node


adjustVariable :: Maybe Int -> Maybe Int -> VarType -> VarType
adjustVariable (Just x) (Just y) (LocalVar n _ _) = LocalVar n (x * SymTab.memOffset) y
adjustVariable (Just x) Nothing (LocalVar n _ sp) =
        LocalVar n (x * SymTab.memOffset) (sp + (x * (-SymTab.memOffset)))
adjustVariable Nothing (Just y) (LocalVar n m _)  = LocalVar n m y
adjustVariable (Just x) _ (ParamVar n _)          = ParamVar n x
adjustVariable (Just x) _ (GlobalVar l _)         = GlobalVar l x
adjustVariable _ _ varType                        = varType
