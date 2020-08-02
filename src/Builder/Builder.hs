{-|
Module       : Builder
Description  : Build assembly code

Builds the output assembly code based on the assembly schema
-}
module Builder.Builder (build) where


import           Control.Monad.Extra    (concatMapM)

import qualified Builder.BuildBinary    as BuildBinary (binary)
import qualified Builder.BuildFunction  as BuildFunction (funcEpilogue,
                                                          funcPrologue,
                                                          functionCall)
import           Builder.BuildState     (BuildState, runBuildState, throwError)
import qualified Builder.BuildState     as BuildState (getState, startState)
import qualified Builder.BuildStatement as BuildStatement (breakStatement,
                                                           continueStatement,
                                                           doWhile,
                                                           emptyStatement,
                                                           forLoop, ifStatement,
                                                           while)
import qualified Builder.BuildTernary   as BuildTernary (ternary)
import qualified Builder.BuildUnary     as BuildUnary (unary)
import qualified Builder.BuildVariables as BuildVariables (addressOf,
                                                           declareGlobal,
                                                           derefLoad,
                                                           derefStore,
                                                           loadLiteral,
                                                           loadVariable,
                                                           outputInit,
                                                           postDeclareAction,
                                                           storeVariable)
import qualified Builder.SchemaCheck    as SchemaCheck (getExpressionSchema,
                                                        getFunctions,
                                                        getInitialisedInt,
                                                        getPointersAssignmentsForInit,
                                                        getStatementSchema,
                                                        getUninitialised)
import qualified Optimiser.Optimiser    as Optimiser (optimise)
import           Types.AssemblySchema
import           Types.Error            (CompilerError (FatalError),
                                         FatalError (BuilderBug))
import           Types.Operator         (BinaryOp (..))
import           Types.Optimise         (Optimise (..))
import           Types.Type             (Type (..))
import           Types.Variables        (Scope (..), VarType (..))


-- | Builds output assembly code
build :: AssemblySchema -> Either CompilerError String
build schema = runBuildState processSchema schema BuildState.startState


processSchema :: AssemblySchema -> BuildState String
processSchema schema = do
        optimiseState <- BuildState.getState
        case optimiseState of
             OptimiseOff -> buildASM schema
             OptimiseOn  -> buildASM . Optimiser.optimise $ schema


buildASM :: AssemblySchema -> BuildState String

buildASM (ProgramSchema topLevelItems) = do
        dataSection <- concatMapM processSchema initialised
        bssSection  <- concatMapM processSchema uninitialised
        initSection <- BuildVariables.outputInit <$> concatMapM processSchema pointersToInit
        textSection <- concatMapM processSchema functions
        pure $ initSection ++ dataSection ++ bssSection ++ textSection
        where
                initialised    = SchemaCheck.getInitialisedInt topLevelItems
                uninitialised  = SchemaCheck.getUninitialised topLevelItems
                pointersToInit = SchemaCheck.getPointersAssignmentsForInit topLevelItems
                functions      = SchemaCheck.getFunctions topLevelItems

buildASM (FunctionSchema name bodyBlock) = do
        body <- processSchema bodyBlock
        pure $ BuildFunction.funcPrologue name ++ body

buildASM (DeclarationSchema
          (ExpressionSchema (VariableSchema global@GlobalVar{}))
          SkipSchema
          Global
          _
         ) = pure $ BuildVariables.declareGlobal global 0

buildASM (DeclarationSchema
          (ExpressionSchema VariableSchema{})
          SkipSchema
          Local
          _
         ) = pure BuildStatement.emptyStatement

buildASM (DeclarationSchema
          (ExpressionSchema (VariableSchema varType))
          (StatementSchema assignSchema)
          _
          _
         ) = do
                 assignAsm <- processStatement assignSchema
                 pure $ assignAsm ++ BuildVariables.postDeclareAction varType

buildASM (DeclarationSchema
          (ExpressionSchema VariableSchema{})
          (ExpressionSchema arrayItems@(ArrayItemsSchema finalPos _))
          Local
          IntArray
         ) = do
                 assignAsm <- processExpression arrayItems
                 pure $ assignAsm ++ BuildVariables.postDeclareAction (LocalVar 0 0 finalPos)

buildASM (StatementSchema statement) = processStatement statement

buildASM (ExpressionSchema expression) = processExpression expression

buildASM SkipSchema = pure BuildStatement.emptyStatement

buildASM schema = throwError $ FatalError (BuilderBug schema)


processStatement :: StatementSchema -> BuildState String
processStatement schema = do
        optimiseState <- BuildState.getState
        case optimiseState of
             OptimiseOff -> buildStatementASM schema
             OptimiseOn  -> buildStatementASM
                            . SchemaCheck.getStatementSchema
                            . Optimiser.optimise $ StatementSchema schema


buildStatementASM :: StatementSchema -> BuildState String

buildStatementASM (ReturnSchema expression) = do
        returnAsm <- processExpression expression
        pure $ returnAsm ++ BuildFunction.funcEpilogue

buildStatementASM (CompoundStatementSchema items) = concatMapM processSchema items

buildStatementASM (BreakSchema (LocalLabel n)) = pure $ BuildStatement.breakStatement n

buildStatementASM (ContinueSchema (LocalLabel n)) = pure $ BuildStatement.continueStatement n

buildStatementASM (AssignmentSchema
                   (VariableSchema globalVar@GlobalVar{})
                   (LiteralSchema n)
                   Global
                  ) = pure $ BuildVariables.declareGlobal globalVar n

buildStatementASM (AssignmentSchema
                   (VariableSchema varType)
                   derefSchema@DereferenceSchema{}
                   _
                  ) = do
                          valueAsm <- processExpression derefSchema
                          pure $ valueAsm ++ BuildVariables.derefStore varType

buildStatementASM (AssignmentSchema
                   (VariableSchema varType)
                   valSchema
                   _
                  ) = do
                          valueAsm <- processExpression valSchema
                          pure $ valueAsm ++ BuildVariables.storeVariable varType

buildStatementASM (AssignmentSchema
                   (DereferenceSchema (VariableSchema varType))
                   valSchema
                   _
                  ) = do
                          valueAsm <- processExpression valSchema
                          pure $ valueAsm ++ BuildVariables.derefStore varType

buildStatementASM (WhileSchema
                   expressionSchema
                   statementSchema
                   (LocalLabel n)
                   (LocalLabel m)
                  ) = do
                          expressionAsm <- processExpression expressionSchema
                          statementAsm  <- processStatement statementSchema
                          pure $ BuildStatement.while expressionAsm statementAsm n m

buildStatementASM (DoWhileSchema
                   statementsSchema
                   expressionSchema
                   (LocalLabel n)
                   (LocalLabel m)
                   (LocalLabel p)
                  ) = do
                          statementAsm  <- processStatement statementsSchema
                          expressionAsm <- processExpression expressionSchema
                          pure $ BuildStatement.doWhile statementAsm expressionAsm n m p

buildStatementASM (ForSchema
                   initSchema
                   testSchema
                   iterSchema
                   bodySchema
                   (LocalLabel n)
                   (LocalLabel m)
                   (LocalLabel p)
                  ) = do
                          initAsm <- processSchema initSchema
                          testAsm <- processExpression testSchema
                          iterAsm <- processExpression iterSchema
                          bodyAsm <- processStatement bodySchema
                          pure $ BuildStatement.forLoop initAsm testAsm iterAsm bodyAsm n m p

buildStatementASM (IfSchema
                   testSchema
                   bodySchema
                   elseSchema
                   (LocalLabel n)
                   (LocalLabel m)
                  ) = do
                          testAsm <- processExpression testSchema
                          bodyAsm <- processStatement bodySchema
                          elseAsm <- processSchema elseSchema
                          pure $ BuildStatement.ifStatement testAsm bodyAsm elseAsm n m

buildStatementASM NullStatementSchema{} = pure BuildStatement.emptyStatement

buildStatementASM schema = throwError $ FatalError (BuilderBug $ StatementSchema schema)


processExpression :: ExpressionSchema -> BuildState String
processExpression schema = do
        optimiseState <- BuildState.getState
        case optimiseState of
             OptimiseOff -> buildExpressionASM schema
             OptimiseOn  -> buildExpressionASM
                            . SchemaCheck.getExpressionSchema
                            . Optimiser.optimise $ ExpressionSchema schema


buildExpressionASM :: ExpressionSchema -> BuildState String

buildExpressionASM (LiteralSchema n) = pure $ BuildVariables.loadLiteral n

buildExpressionASM (UnarySchema varSchema@(VariableSchema varType) operator) = do
        expressionAsm <- processExpression varSchema
        pure $ expressionAsm ++ BuildUnary.unary operator varType

buildExpressionASM (UnarySchema expressionSchema operator) = do
        expressionAsm <- processExpression expressionSchema
        pure $ expressionAsm ++ BuildUnary.unary operator (LocalVar 0 0 0)

buildExpressionASM (BinarySchema
                    exprSchema1
                    (LiteralSchema n)
                    op@ShiftOp{}
                    (LocalLabel m)
                    (LocalLabel p)
                   ) = do
                           expr1Asm <- processExpression exprSchema1
                           pure $ BuildBinary.binary expr1Asm (show n) op m p

buildExpressionASM (BinarySchema
                    exprSchema1
                    exprSchema2
                    op
                    (LocalLabel n)
                    (LocalLabel m)
                   ) = do
                           expr1Asm <- processExpression exprSchema1
                           expr2Asm <- processExpression exprSchema2
                           pure $ BuildBinary.binary expr1Asm expr2Asm op n m

buildExpressionASM (TernarySchema
                    expr1
                    expr2
                    expr3
                    (LocalLabel n)
                    (LocalLabel m)
                   ) = do
                           expr1Asm <- processExpression expr1
                           expr2Asm <- processExpression expr2
                           expr3Asm <- processExpression expr3
                           pure $ BuildTernary.ternary expr1Asm expr2Asm expr3Asm n m

buildExpressionASM (FunctionCallSchema name arguments) = do
        argAsmList <- mapM processExpression arguments
        pure $ BuildFunction.functionCall name (zip argAsmList [0..])

buildExpressionASM (ExpressionStatementSchema statementSchema) =
        processStatement statementSchema

buildExpressionASM (VariableSchema varType) =
        pure $ BuildVariables.loadVariable varType

buildExpressionASM (AddressOfSchema (VariableSchema varType)) =
        pure $ BuildVariables.addressOf varType

buildExpressionASM (DereferenceSchema (VariableSchema varType)) =
        pure $ BuildVariables.derefLoad varType

buildExpressionASM NullExpressionSchema{} = pure BuildStatement.emptyStatement

buildExpressionASM (ArrayItemsSchema _ items) = concatMapM processStatement items

buildExpressionASM schema = throwError $ FatalError (BuilderBug $ ExpressionSchema schema)
