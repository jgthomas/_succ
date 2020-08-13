{-|
Module       : Converter
Description  : Convert syntax tree to assembly schema

Converts an abstract syntax tree into an assembly schema
-}
module Converter.Converter (convert) where


import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe)

import qualified State.FuncState      as FuncState
import           State.GenState       (GenState, runGenState, throwError)
import qualified State.GenState       as GenState (getState, startState)
import qualified State.GlobalState    as GlobalState
import           State.State          (SymTab)
import qualified State.State          as State (getScope, getVariable, labelNum,
                                                memOffset)
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
        FuncState.initFunction name
        bodySchema <- checkReturn name <$> convertToSchema body
        FuncState.closeFunction
        GlobalState.defineFunction name
        pure (FunctionSchema name bodySchema)

convertToSchema (ParamNode typ (VarNode name _) _) = do
        FuncState.addParameter name typ
        pure SkipSchema

convertToSchema (FuncCallNode name argList _) = do
        argPosValues <- argsToPosValue argList
        argSchemas   <- mapM convertToSchema argList
        FuncState.paramValuesFromArgs name argPosValues
        pure (ExpressionSchema $ FunctionCallSchema name argSchemas)

convertToSchema (ArgNode arg _) = convertToSchema arg

convertToSchema (CompoundStmtNode statements _) = do
        FuncState.initScope
        statementsSchema <- mapM convertToSchema statements
        FuncState.closeScope
        pure (StatementSchema $ CompoundStatementSchema statementsSchema)

convertToSchema (ForLoopNode ini test iter body _) = do
        FuncState.initScope
        passLabel <- State.labelNum
        failLabel <- State.labelNum
        contLabel <- State.labelNum
        FuncState.setBreak failLabel
        FuncState.setContinue contLabel
        iniSchema  <- convertToSchema ini
        testSchema <- convertToSchema test
        iterSchema <- convertToSchema iter
        bodySchema <- convertToSchema body
        FuncState.closeScope
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
        loopLabel  <- State.labelNum
        testLabel  <- State.labelNum
        FuncState.setContinue loopLabel
        FuncState.setBreak testLabel
        testSchema <- convertToSchema test
        bodySchema <- convertToSchema body
        pure (StatementSchema
              (WhileSchema
               testSchema
               bodySchema
               (LocalLabel loopLabel)
               (LocalLabel testLabel)
              )
             )

convertToSchema (DoWhileNode body test _) = do
        loopLabel  <- State.labelNum
        contLabel  <- State.labelNum
        testLabel  <- State.labelNum
        FuncState.setContinue contLabel
        FuncState.setBreak testLabel
        bodySchema <- convertToSchema body
        testSchema <- convertToSchema test
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
        ifLabel    <- LocalLabel <$> State.labelNum
        elseLabel  <- LocalLabel <$> State.labelNum
        testSchema <- convertToSchema test
        bodySchema <- convertToSchema body
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
        currScope <- State.getScope
        case currScope of
             Local  -> declareLocal node
             Global -> declareGlobal node

convertToSchema node@AssignmentNode{} = buildAssignmentSchema node

convertToSchema (AssignDereferenceNode varNode valNode operator dat) =
        convertToSchema (AssignmentNode varNode valNode operator dat)

convertToSchema (ExprStmtNode exprStatement _) = convertToSchema exprStatement

convertToSchema ContinueNode{} = do
        contineLabel <- LocalLabel . fromMaybe (-1) <$> FuncState.getContinue
        pure (StatementSchema $ ContinueSchema contineLabel)

convertToSchema BreakNode{} = do
        breakLabel <- LocalLabel . fromMaybe (-1) <$> FuncState.getBreak
        pure (StatementSchema $ BreakSchema breakLabel)

convertToSchema (ReturnNode val _) = do
        valueSchema <- convertToSchema val
        pure (StatementSchema $ ReturnSchema valueSchema)

convertToSchema (TernaryNode test true false _) = do
        testSchema  <- convertToSchema test
        trueSchema  <- convertToSchema true
        falseSchema <- convertToSchema false
        trueLabel   <- LocalLabel <$> State.labelNum
        falseLabel  <- LocalLabel <$> State.labelNum
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
        trueLabel   <- LocalLabel <$> State.labelNum
        falseLabel  <- LocalLabel <$> State.labelNum
        leftSchema  <- convertToSchema leftNode
        rightSchema <- convertToSchema rightNode
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
        value <- convertToSchema val
        pure (ExpressionSchema $ UnarySchema value unOp)

convertToSchema (ConstantNode n _) = pure (ExpressionSchema $ LiteralSchema n)

convertToSchema node@(VarNode name _) = do
        varType <- State.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType typ -> pure (ExpressionSchema $ VariableSchema typ)

convertToSchema node@(DereferenceNode name _) = do
        varType <- State.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType var -> pure (ExpressionSchema
                                  (DereferenceSchema
                                   (ExpressionSchema $ VariableSchema var)
                                  )
                                 )

convertToSchema node@(AddressOfNode name _) = do
        varType <- State.getVariable name
        case varType of
             NotFound    -> throwError $ FatalError (ConverterBug node)
             VarType var -> pure (ExpressionSchema
                                  (AddressOfSchema
                                   (ExpressionSchema $ VariableSchema var)
                                  )
                                 )

convertToSchema NullExprNode{} = pure SkipSchema

convertToSchema (ArrayNode arrayNode) = convertToSchemaArray arrayNode

convertToSchema node = throwError $ FatalError (ConverterBug node)


convertToSchemaArray :: ArrayNode -> GenState AssemblySchema

convertToSchemaArray (ArrayDeclareNode len var typ Nothing dat) = do
        declareSchema <- convertToSchema (DeclarationNode var typ Nothing dat)
        FuncState.incrementOffsetByN (len - 1)
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
getArrayIndexItem pos varNode@VarNode{} = setSchemaOffset pos <$> convertToSchema varNode
getArrayIndexItem _ node = throwError $ FatalError (ConverterBug node)


processArrayItems :: Tree -> [Tree] -> GenState AssemblySchema
processArrayItems varNode items = do
        arrayItemsSchema <- mapM (processArrayItem varNode) (zip items [0..])
        FuncState.incrementOffsetByN (length items - 1)
        adjust           <- FuncState.stackPointerValue
        pure (StatementSchema
              (ArrayItemsSchema
               adjust
               arrayItemsSchema
              )
             )


processArrayItem :: Tree -> (Tree, Int) -> GenState AssemblySchema
processArrayItem varNode (item, pos) = do
        currScope <- State.getScope
        varSchema <- getArrayIndexItem pos varNode
        valSchema <- convertToSchema item
        pure (StatementSchema
              (AssignmentSchema
               varSchema
               valSchema
               currScope
              )
             )


setSchemaOffset :: Int -> AssemblySchema -> AssemblySchema
setSchemaOffset n (ExpressionSchema (VariableSchema varType)) =
        ExpressionSchema $ VariableSchema $ adjustVariable (Just n) Nothing varType
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
           do FuncState.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ FatalError (ConverterBug tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        FuncState.initFunction name
        mapM_ convertToSchema params
        FuncState.closeFunction


argsToPosValue :: [Tree] -> GenState [(Int, VarValue)]
argsToPosValue argList = zip [0..] <$> mapM argToValue argList


argToValue :: Tree -> GenState VarValue
argToValue (ArgNode valNode _) = pure $ buildVarableValue valNode
argToValue tree                = throwError $ FatalError (ConverterBug tree)



checkReturn :: String -> AssemblySchema -> AssemblySchema
checkReturn "main" (StatementSchema (CompoundStatementSchema [])) =
        StatementSchema (CompoundStatementSchema $ addReturnZero [])
checkReturn "main" schema@(StatementSchema (CompoundStatementSchema bodySchemas)) =
        case last bodySchemas of
             (StatementSchema ReturnSchema{}) -> schema
             _ -> StatementSchema (CompoundStatementSchema $ addReturnZero bodySchemas)
checkReturn _ schema = schema


addReturnZero :: [AssemblySchema] -> [AssemblySchema]
addReturnZero bodySchema = bodySchema ++ [StatementSchema
                                          (ReturnSchema
                                           (ExpressionSchema
                                            (LiteralSchema 0)
                                           )
                                          )
                                         ]


-- Variables Global

declareGlobal :: Tree -> GenState AssemblySchema
declareGlobal (DeclarationNode (VarNode name _) typ Nothing _) = do
        globLab <- GlobalState.makeLabel name
        GlobalState.declareGlobal name typ globLab
        pure SkipSchema
declareGlobal node@(DeclarationNode (VarNode name _) typ _ _) = do
        currLabel <- GlobalState.getLabel name
        case currLabel of
             Just _  -> processGlobalAssignment node
             Nothing -> do
                     globLab <- GlobalState.makeLabel name
                     GlobalState.declareGlobal name typ globLab
                     processGlobalAssignment node
declareGlobal tree = throwError $ FatalError (ConverterBug tree)


processGlobalAssignment :: Tree -> GenState AssemblySchema
processGlobalAssignment (DeclarationNode varNode typ (Just assignNode) _) = do
        currScope    <- State.getScope
        varSchema    <- convertToSchema varNode
        assignSchema <- convertToSchema assignNode
        pure (DeclarationSchema varSchema assignSchema currScope typ)
processGlobalAssignment tree = throwError $ FatalError (ConverterBug tree)


defineGlobal :: Tree -> GenState AssemblySchema
defineGlobal (AssignmentNode varNode@(VarNode name _) valNode _ _) = do
        GlobalState.defineGlobal name
        GlobalState.setValue name (buildVarableValue valNode)
        currScope <- State.getScope
        varSchema <- convertToSchema varNode
        valSchema <- convertToSchema valNode
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
        _ <- FuncState.addVariable name typ
        currScope <- State.getScope
        varSchema <- convertToSchema varNode
        valSchema <- processPossibleNode value
        pure (DeclarationSchema varSchema valSchema currScope typ)
declareLocal tree = throwError $ FatalError (ConverterBug tree)


defineLocal :: Tree -> GenState AssemblySchema
defineLocal (AssignmentNode varNode value _ _) = do
        currScope <- State.getScope
        varSchema <- convertToSchema varNode
        valSchema <- convertToSchema value
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
        currScope <- State.getScope
        case currScope of
             Local  -> defineLocal node
             Global -> defineGlobal node


processPossibleNode :: Maybe Tree -> GenState AssemblySchema
processPossibleNode Nothing     = pure SkipSchema
processPossibleNode (Just node) = convertToSchema node


adjustVariable :: Maybe Int -> Maybe Int -> VarType -> VarType
adjustVariable (Just x) (Just y) (LocalVar n _ _) = LocalVar n (x * State.memOffset) y
adjustVariable (Just x) Nothing (LocalVar n _ sp) =
        LocalVar n (x * State.memOffset) (sp + (x * (-State.memOffset)))
adjustVariable Nothing (Just y) (LocalVar n m _)  = LocalVar n m y
adjustVariable (Just x) _ (ParamVar n _)          = ParamVar n x
adjustVariable (Just x) _ (GlobalVar l _)         = GlobalVar l x
adjustVariable _ _ varType                        = varType


buildVarableValue :: Tree -> VarValue
buildVarableValue (ConstantNode n _) = SingleValue n
buildVarableValue _                  = UntrackedValue
