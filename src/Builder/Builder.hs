
module Builder.Builder (build) where


import Control.Monad.Extra    (concatMapM)

import Builder.BuildBinary    as BuildBinary (binary)
import Builder.BuildFunction  as BuildFunction (funcEpilogue, funcPrologue)
import Builder.BuildState     (BuildState, runBuildState)
import Builder.BuildState     as BuildState (startState)
import Builder.BuildStatement as BuildStatement (doWhile, forLoop, ifStatement,
                                                 while)
import Builder.BuildTernary   as BuildTernary (ternary)
import Builder.BuildUnary     as BuildUnary (unary)
import Builder.BuildVariables as BuildVariables (addressOf, declareGlobal,
                                                 derefLoad, loadLiteral,
                                                 loadVariable,
                                                 postDeclareAction,
                                                 storeVariable)
import Types.AssemblySchema
import Types.Error
import Types.Variables        (Scope (..), VarType (..))


build :: AssemblySchema -> Either CompilerError String
build schema = runBuildState buildAssembly schema BuildState.startState


buildAssembly :: AssemblySchema -> BuildState String
buildAssembly schema = buildASM schema



buildASM :: AssemblySchema -> BuildState String

buildASM (ProgramSchema topLeveltems) = concatMapM buildASM topLeveltems

buildASM (FunctionSchema name bodyBlock) = do
        body <- buildASM bodyBlock
        pure $ prologue ++ body ++ epilogue
        where
                prologue = BuildFunction.funcPrologue name
                epilogue = BuildFunction.funcEpilogue

buildASM (DeclarationSchema _ SkipSchema Global _) = pure ""
buildASM (DeclarationSchema
          (ExpressionSchema (VariableSchema varType))
          (StatementSchema assignSchema) _ _) = do
                  assignAsm <- buildStatementASM assignSchema
                  pure $ assignAsm ++ BuildVariables.postDeclareAction varType

buildASM DeclarationSchema{} = pure ""

buildASM (StatementSchema statement) = buildStatementASM statement

buildASM (ExpressionSchema expression) = buildExpressionASM expression

buildASM SkipSchema = pure ""



buildStatementASM :: StatementSchema -> BuildState String

buildStatementASM (ReturnSchema expression) = buildExpressionASM expression

buildStatementASM (CompoundStatementSchema items) = concatMapM buildASM items

buildStatementASM (AssignmentSchema
                   (VariableSchema globalVar@GlobalVar{})
                   (LiteralSchema n)
                   Global) =
                           pure $ BuildVariables.declareGlobal globalVar n
buildStatementASM (AssignmentSchema
                   (VariableSchema varType)
                   valSchema
                   Local) = do
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

buildStatementASM _                         = pure ""



buildExpressionASM :: ExpressionSchema -> BuildState String

buildExpressionASM (LiteralSchema n) = pure $ loadLiteral n

buildExpressionASM (UnarySchema varSchema@(VariableSchema varType) operator) = do
        expressionAsm <- buildExpressionASM varSchema
        pure $ expressionAsm ++ BuildUnary.unary operator varType
buildExpressionASM (UnarySchema expressionSchema operator) = do
        expressionAsm <- buildExpressionASM expressionSchema
        pure $ expressionAsm ++ BuildUnary.unary operator (LocalVar 0 0 0)

buildExpressionASM (BinarySchema exprSchema1 exprSchema2 op locLabel1 locLabel2) = do
        expr1Asm <- buildExpressionASM exprSchema1
        expr2Asm <- buildExpressionASM exprSchema2
        pure $ BuildBinary.binary expr1Asm expr2Asm op locLabel1 locLabel2

buildExpressionASM (TernarySchema expr1 expr2 expr3 locLabel1 locLabel2) = do
        expr1Asm <- buildExpressionASM expr1
        expr2Asm <- buildExpressionASM expr2
        expr3Asm <- buildExpressionASM expr3
        pure $ BuildTernary.ternary expr1Asm expr2Asm expr3Asm locLabel1 locLabel2

buildExpressionASM (ExpressionStatementSchema statementSchema) =
        buildStatementASM statementSchema

buildExpressionASM (VariableSchema varType) =
        pure $ BuildVariables.loadVariable varType

buildExpressionASM (AddressOfSchema (VariableSchema varType)) =
        pure $ BuildVariables.addressOf varType

buildExpressionASM (DereferenceSchema (VariableSchema varType)) =
        pure $ BuildVariables.derefLoad varType

buildExpressionASM _                 = pure ""
