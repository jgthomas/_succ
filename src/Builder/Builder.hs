
module Builder.Builder (build) where


import Control.Monad.Extra    (concatMapM)

import Builder.BuildBinary    as BuildBinary (binary)
import Builder.BuildFunction  as BuildFunction (funcEpilogue, funcPrologue,
                                                functionCall)
import Builder.BuildState     (BuildState, runBuildState)
import Builder.BuildState     as BuildState (startState)
import Builder.BuildStatement as BuildStatement (breakStatement,
                                                 continueStatement, doWhile,
                                                 forLoop, ifStatement, while)
import Builder.BuildTernary   as BuildTernary (ternary)
import Builder.BuildUnary     as BuildUnary (unary)
import Builder.BuildVariables as BuildVariables (addressOf, declareGlobal,
                                                 derefLoad, derefStore,
                                                 loadLiteral, loadVariable,
                                                 outputInit, postDeclareAction,
                                                 storeVariable)
import Types.AssemblySchema
import Types.Error
import Types.Operator         (BinaryOp (..))
import Types.Type             (Type (..))
import Types.Variables        (Scope (..), VarType (..))


build :: AssemblySchema -> Either CompilerError String
build schema = runBuildState buildAssembly schema BuildState.startState


buildAssembly :: AssemblySchema -> BuildState String
buildAssembly schema = buildASM schema



buildASM :: AssemblySchema -> BuildState String

buildASM (ProgramSchema topLevelItems) = do
        dataSection <- concatMapM buildASM initialised
        bssSection  <- concatMapM buildASM uninitialised
        initSection <- BuildVariables.outputInit <$> concatMapM buildASM pointersToInit
        textSection <- concatMapM buildASM functions
        pure $ initSection ++ dataSection ++ bssSection ++ textSection
        where
                initialised    = getInitialisedInt topLevelItems
                uninitialised  = getUninitialised topLevelItems
                pointersToInit = getPointersAssignmentsForInit topLevelItems
                functions      = getFunctions topLevelItems

buildASM (FunctionSchema name bodyBlock) = do
        body <- buildASM bodyBlock
        pure $ BuildFunction.funcPrologue name ++ body

buildASM (DeclarationSchema
          (ExpressionSchema (VariableSchema global@GlobalVar{}))
          SkipSchema
          Global
          _) = pure $ BuildVariables.declareGlobal global 0
buildASM (DeclarationSchema
          (ExpressionSchema (VariableSchema varType))
          (StatementSchema assignSchema)
          _
          _
         ) = do
                 assignAsm <- buildStatementASM assignSchema
                 pure $ assignAsm ++ BuildVariables.postDeclareAction varType
buildASM (DeclarationSchema
          (ExpressionSchema VariableSchema{})
          (ExpressionSchema arrayItems@(ArrayItemsSchema finalPos _))
          Local
          IntArray
         ) = do
                 assignAsm <- buildExpressionASM arrayItems
                 pure $ assignAsm ++ BuildVariables.postDeclareAction (LocalVar 0 0 finalPos)

buildASM DeclarationSchema{} = pure ""

buildASM (StatementSchema statement) = buildStatementASM statement

buildASM (ExpressionSchema expression) = buildExpressionASM expression

buildASM SkipSchema = pure ""



buildStatementASM :: StatementSchema -> BuildState String

buildStatementASM (ReturnSchema expression) = do
        returnAsm <- buildExpressionASM expression
        pure $ returnAsm ++ BuildFunction.funcEpilogue

buildStatementASM (CompoundStatementSchema items) = concatMapM buildASM items

buildStatementASM (BreakSchema (LocalLabel n)) = pure $ BuildStatement.breakStatement n

buildStatementASM (ContinueSchema (LocalLabel n)) = pure $ BuildStatement.continueStatement n

buildStatementASM (AssignmentSchema
                   (VariableSchema globalVar@GlobalVar{})
                   (LiteralSchema n)
                   Global) =
                           pure $ BuildVariables.declareGlobal globalVar n
buildStatementASM (AssignmentSchema
                   (VariableSchema varType)
                   derefSchema@DereferenceSchema{}
                   _
                  ) = do
                          valueAsm <- buildExpressionASM derefSchema
                          pure $ valueAsm ++ BuildVariables.derefStore varType
buildStatementASM (AssignmentSchema
                   (VariableSchema varType)
                   valSchema
                   _
                  ) = do
                          valueAsm <- buildExpressionASM valSchema
                          pure $ valueAsm ++ BuildVariables.storeVariable varType

buildStatementASM (WhileSchema
                   expressionSchema
                   statementSchema
                   (LocalLabel n)
                   (LocalLabel m)) = do
                           expressionAsm <- buildExpressionASM expressionSchema
                           statementAsm  <- buildStatementASM statementSchema
                           pure $ BuildStatement.while expressionAsm statementAsm n m

buildStatementASM (DoWhileSchema
                   statementsSchema
                   expressionSchema
                   (LocalLabel n)
                   (LocalLabel m)
                   (LocalLabel p)) = do
                           statementAsm  <- buildStatementASM statementsSchema
                           expressionAsm <- buildExpressionASM expressionSchema
                           pure $ BuildStatement.doWhile statementAsm expressionAsm n m p

buildStatementASM (ForSchema
                   initSchema
                   testSchema
                   iterSchema
                   bodySchema
                   (LocalLabel n)
                   (LocalLabel m)
                   (LocalLabel p)) = do
                           initAsm <- buildASM initSchema
                           testAsm <- buildExpressionASM testSchema
                           iterAsm <- buildExpressionASM iterSchema
                           bodyAsm <- buildStatementASM bodySchema
                           pure $ BuildStatement.forLoop initAsm testAsm iterAsm bodyAsm n m p

buildStatementASM (IfSchema
                   testSchema
                   bodySchema
                   elseSchema
                   (LocalLabel n)
                   (LocalLabel m)) = do
                           testAsm <- buildExpressionASM testSchema
                           bodyAsm <- buildStatementASM bodySchema
                           elseAsm <- buildASM elseSchema
                           pure $ BuildStatement.ifStatement testAsm bodyAsm elseAsm n m

buildStatementASM NullStatementSchema{} = pure ""

buildStatementASM _                         = pure ""



buildExpressionASM :: ExpressionSchema -> BuildState String

buildExpressionASM (LiteralSchema n) = pure $ loadLiteral n

buildExpressionASM (UnarySchema varSchema@(VariableSchema varType) operator) = do
        expressionAsm <- buildExpressionASM varSchema
        pure $ expressionAsm ++ BuildUnary.unary operator varType
buildExpressionASM (UnarySchema expressionSchema operator) = do
        expressionAsm <- buildExpressionASM expressionSchema
        pure $ expressionAsm ++ BuildUnary.unary operator (LocalVar 0 0 0)

buildExpressionASM (BinarySchema
                    exprSchema1
                    (LiteralSchema n)
                    op@ShiftOp{}
                    locLabel1
                    locLabel2) = do
                            expr1Asm <- buildExpressionASM exprSchema1
                            pure $ BuildBinary.binary expr1Asm (show n) op locLabel1 locLabel2
buildExpressionASM (BinarySchema
                    exprSchema1
                    exprSchema2
                    op
                    locLabel1
                    locLabel2) = do
                            expr1Asm <- buildExpressionASM exprSchema1
                            expr2Asm <- buildExpressionASM exprSchema2
                            pure $ BuildBinary.binary expr1Asm expr2Asm op locLabel1 locLabel2

buildExpressionASM (TernarySchema expr1 expr2 expr3 locLabel1 locLabel2) = do
        expr1Asm <- buildExpressionASM expr1
        expr2Asm <- buildExpressionASM expr2
        expr3Asm <- buildExpressionASM expr3
        pure $ BuildTernary.ternary expr1Asm expr2Asm expr3Asm locLabel1 locLabel2

buildExpressionASM (FunctionCallSchema name arguments) = do
        argAsmList <- mapM buildExpressionASM arguments
        pure $ BuildFunction.functionCall name (zip argAsmList [0..])

buildExpressionASM (ExpressionStatementSchema statementSchema) =
        buildStatementASM statementSchema

buildExpressionASM (VariableSchema varType) =
        pure $ BuildVariables.loadVariable varType

buildExpressionASM (AddressOfSchema (VariableSchema varType)) =
        pure $ BuildVariables.addressOf varType

buildExpressionASM (DereferenceSchema (VariableSchema varType)) =
        pure $ BuildVariables.derefLoad varType

buildExpressionASM NullExpressionSchema{} = pure ""

buildExpressionASM (ArrayItemsSchema _ items) = concatMapM buildStatementASM items

buildExpressionASM _                 = pure ""


getFunctions :: [AssemblySchema] -> [AssemblySchema]
getFunctions items = filter isFunction items


getInitialisedInt :: [AssemblySchema] -> [AssemblySchema]
getInitialisedInt items = filter isInitialisedInt items


getUninitialised :: [AssemblySchema] -> [AssemblySchema]
getUninitialised items = map convertForInit . filter needsInit $ items


getPointersAssignmentsForInit :: [AssemblySchema] -> [AssemblySchema]
getPointersAssignmentsForInit items = filter isInitialisedPointer items


isFunction :: AssemblySchema -> Bool
isFunction FunctionSchema{} = True
isFunction _                = False


isInitialisedInt :: AssemblySchema -> Bool
isInitialisedInt (DeclarationSchema _ SkipSchema _ _)                                                 = False
isInitialisedInt (DeclarationSchema _ (StatementSchema (AssignmentSchema _ AddressOfSchema{} _)) _ _) = False
isInitialisedInt DeclarationSchema{}                                                                  = True
isInitialisedInt _                                                                                    = False


needsInit :: AssemblySchema -> Bool
needsInit (DeclarationSchema _ SkipSchema _ _) = True
needsInit schema                               = isInitialisedPointer schema


convertForInit :: AssemblySchema -> AssemblySchema
convertForInit schema@(DeclarationSchema _ SkipSchema _ _) = schema
convertForInit (DeclarationSchema
                varSchema
                (StatementSchema (AssignmentSchema _ AddressOfSchema{} _))
                scope
                typ
               ) = DeclarationSchema varSchema SkipSchema scope typ
convertForInit schema = schema


isInitialisedPointer :: AssemblySchema -> Bool
isInitialisedPointer (DeclarationSchema _ (StatementSchema (AssignmentSchema _ AddressOfSchema{} _)) _ _) = True
isInitialisedPointer _                                                                                    = False
