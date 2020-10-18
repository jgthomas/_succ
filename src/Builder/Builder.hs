-- |
-- Module       : Builder
-- Description  : Build assembly code
--
-- Builds the output assembly code based on the assembly schema.
module Builder.Builder
  ( build,
  )
where

import qualified Builder.BuildBinary as BuildBinary (binary)
import qualified Builder.BuildFunction as BuildFunction
  ( funcEpilogue,
    funcPrologue,
    functionCall,
  )
import Builder.BuildState (BuildState, throwError)
import qualified Builder.BuildState as BuildState (evaluate, getState)
import qualified Builder.BuildStatement as BuildStatement
  ( breakStatement,
    continueStatement,
    doWhile,
    emptyStatement,
    forLoop,
    ifStatement,
    while,
  )
import qualified Builder.BuildTernary as BuildTernary (ternary)
import qualified Builder.BuildUnary as BuildUnary (unary)
import qualified Builder.BuildVariables as BuildVariables
  ( addressOf,
    declareGlobal,
    derefLoad,
    derefStore,
    loadLiteral,
    loadVariable,
    outputInit,
    postDeclareAction,
    storeVariable,
  )
import qualified Builder.SchemaFilter as SchemaFilter (filterSchema)
import Control.Monad.Extra (concatMapM)
import qualified Optimiser.Optimiser as Optimiser (optimise)
import Types.AssemblySchema
import Types.Error
  ( CompilerError (FatalError),
    FatalError (BuilderBug),
  )
import Types.Operator (BinaryOp (..))
import Types.SuccTokens
  ( Optimise (..),
    SuccOptions (..),
    TopLevelItem (..),
  )
import Types.Type (Type (..))
import Types.Variables (Scope (..), VarType (..))

-- | Builds output assembly code
build :: SuccOptions -> AssemblySchema -> Either CompilerError String
build options schema = BuildState.evaluate processSchema schema options

processSchema :: AssemblySchema -> BuildState String
processSchema schema = do
  optimiseState <- optimiseSet <$> BuildState.getState
  case optimiseState of
    OptimiseOff -> buildASM schema
    OptimiseOn -> buildASM . Optimiser.optimise $ schema

buildASM :: AssemblySchema -> BuildState String
buildASM (ProgramSchema topLevelItems) = do
  dataSection <- concatMapM processSchema initialised
  bssSection <- concatMapM processSchema uninitialised
  initSection <- BuildVariables.outputInit <$> concatMapM processSchema pointersToInit
  textSection <- concatMapM processSchema functions
  pure $ initSection ++ dataSection ++ bssSection ++ textSection
  where
    initialised = SchemaFilter.filterSchema InitialisedVariable topLevelItems
    uninitialised = SchemaFilter.filterSchema UninitialisedVariable topLevelItems
    pointersToInit = SchemaFilter.filterSchema InitialisedPointer topLevelItems
    functions = SchemaFilter.filterSchema Function topLevelItems
buildASM (FunctionSchema name bodyBlock) = do
  body <- processSchema bodyBlock
  pure $ BuildFunction.funcPrologue name ++ body
buildASM
  ( DeclarationSchema
      (ExpressionSchema (VariableSchema global@GlobalVar {} _))
      SkipSchema
      Global
      _
    ) = pure $ BuildVariables.declareGlobal global [0]
buildASM
  ( DeclarationSchema
      (ExpressionSchema (VariableSchema global@GlobalVar {} _))
      (StatementSchema ArrayItemsSchema {})
      Global
      IntArray
    ) = pure $ BuildVariables.declareGlobal global [1, 7]
buildASM
  ( DeclarationSchema
      (ExpressionSchema VariableSchema {})
      SkipSchema
      Local
      _
    ) = pure BuildStatement.emptyStatement
buildASM
  ( DeclarationSchema
      (ExpressionSchema VariableSchema {})
      arrayItems@(StatementSchema (ArrayItemsSchema finalPos _))
      Local
      IntArray
    ) = do
    assignAsm <- processSchema arrayItems
    pure $ assignAsm ++ BuildVariables.postDeclareAction (LocalVar 0 0 finalPos)
buildASM
  ( DeclarationSchema
      (ExpressionSchema (VariableSchema varType _))
      assign@(StatementSchema _)
      _
      _
    ) = do
    assignAsm <- processSchema assign
    pure $ assignAsm ++ BuildVariables.postDeclareAction varType
buildASM (StatementSchema statement) = buildStatementASM statement
buildASM (ExpressionSchema expression) = buildExpressionASM expression
buildASM SkipSchema = pure BuildStatement.emptyStatement
buildASM schema = throwError $ FatalError (BuilderBug schema)

buildStatementASM :: StatementSchema -> BuildState String
buildStatementASM (ReturnSchema expression) = do
  returnAsm <- processSchema expression
  pure $ returnAsm ++ BuildFunction.funcEpilogue
buildStatementASM (CompoundStatementSchema items) = concatMapM processSchema items
buildStatementASM (BreakSchema (LocalLabel n)) = pure $ BuildStatement.breakStatement n
buildStatementASM (ContinueSchema (LocalLabel n)) = pure $ BuildStatement.continueStatement n
buildStatementASM
  ( AssignmentSchema
      (ExpressionSchema (VariableSchema globalVar@GlobalVar {} _))
      (ExpressionSchema (LiteralSchema n))
      Global
    ) = pure $ BuildVariables.declareGlobal globalVar [n]
buildStatementASM
  ( AssignmentSchema
      (ExpressionSchema (VariableSchema varType _))
      derefSchema@(ExpressionSchema DereferenceSchema {})
      _
    ) = do
    valueAsm <- processSchema derefSchema
    pure $ valueAsm ++ BuildVariables.derefStore varType
buildStatementASM
  ( AssignmentSchema
      (ExpressionSchema (VariableSchema varType _))
      valSchema
      _
    ) = do
    valueAsm <- processSchema valSchema
    pure $ valueAsm ++ BuildVariables.storeVariable varType
buildStatementASM
  ( AssignmentSchema
      (ExpressionSchema (DereferenceSchema (ExpressionSchema (VariableSchema varType _))))
      valSchema
      _
    ) = do
    valueAsm <- processSchema valSchema
    pure $ valueAsm ++ BuildVariables.derefStore varType
buildStatementASM
  ( WhileSchema
      expressionSchema
      statementSchema
      (LocalLabel n)
      (LocalLabel m)
    ) = do
    expressionAsm <- processSchema expressionSchema
    statementAsm <- processSchema statementSchema
    pure $ BuildStatement.while expressionAsm statementAsm n m
buildStatementASM
  ( DoWhileSchema
      statementsSchema
      expressionSchema
      (LocalLabel n)
      (LocalLabel m)
      (LocalLabel p)
    ) = do
    statementAsm <- processSchema statementsSchema
    expressionAsm <- processSchema expressionSchema
    pure $ BuildStatement.doWhile statementAsm expressionAsm n m p
buildStatementASM
  ( ForSchema
      initSchema
      testSchema
      iterSchema
      bodySchema
      (LocalLabel n)
      (LocalLabel m)
      (LocalLabel p)
    ) = do
    initAsm <- processSchema initSchema
    testAsm <- processSchema testSchema
    iterAsm <- processSchema iterSchema
    bodyAsm <- processSchema bodySchema
    pure $ BuildStatement.forLoop initAsm testAsm iterAsm bodyAsm n m p
buildStatementASM
  ( IfSchema
      testSchema
      bodySchema
      elseSchema
      (LocalLabel n)
      (LocalLabel m)
    ) = do
    testAsm <- processSchema testSchema
    bodyAsm <- processSchema bodySchema
    elseAsm <- processSchema elseSchema
    pure $ BuildStatement.ifStatement testAsm bodyAsm elseAsm n m
buildStatementASM (ArrayItemsSchema _ items) = concatMapM processSchema items
buildStatementASM schema = throwError $ FatalError (BuilderBug $ StatementSchema schema)

buildExpressionASM :: ExpressionSchema -> BuildState String
buildExpressionASM (LiteralSchema n) = pure $ BuildVariables.loadLiteral n
buildExpressionASM (UnarySchema varSchema@(ExpressionSchema (VariableSchema varType _)) operator) = do
  expressionAsm <- processSchema varSchema
  pure $ expressionAsm ++ BuildUnary.unary operator varType
buildExpressionASM (UnarySchema expressionSchema operator) = do
  expressionAsm <- processSchema expressionSchema
  pure $ expressionAsm ++ BuildUnary.unary operator (LocalVar 0 0 0)
buildExpressionASM
  ( BinarySchema
      exprSchema1
      (ExpressionSchema (LiteralSchema n))
      op@ShiftOp {}
      (LocalLabel m)
      (LocalLabel p)
    ) = do
    expr1Asm <- processSchema exprSchema1
    pure $ BuildBinary.binary expr1Asm (show n) op m p
buildExpressionASM
  ( BinarySchema
      exprSchema1
      exprSchema2
      op
      (LocalLabel n)
      (LocalLabel m)
    ) = do
    expr1Asm <- processSchema exprSchema1
    expr2Asm <- processSchema exprSchema2
    pure $ BuildBinary.binary expr1Asm expr2Asm op n m
buildExpressionASM
  ( TernarySchema
      expr1
      expr2
      expr3
      (LocalLabel n)
      (LocalLabel m)
    ) = do
    expr1Asm <- processSchema expr1
    expr2Asm <- processSchema expr2
    expr3Asm <- processSchema expr3
    pure $ BuildTernary.ternary expr1Asm expr2Asm expr3Asm n m
buildExpressionASM (FunctionCallSchema name arguments) = do
  argAsmList <- mapM processSchema arguments
  pure $ BuildFunction.functionCall name (zip argAsmList [0 ..])
buildExpressionASM (VariableSchema varType _) =
  pure $ BuildVariables.loadVariable varType
buildExpressionASM (AddressOfSchema (ExpressionSchema (VariableSchema varType _))) =
  pure $ BuildVariables.addressOf varType
buildExpressionASM (DereferenceSchema (ExpressionSchema (VariableSchema varType _))) =
  pure $ BuildVariables.derefLoad varType
buildExpressionASM schema = throwError $ FatalError (BuilderBug $ ExpressionSchema schema)
