
module Builder.Builder (build) where


import Control.Monad.Extra    (concatMapM)

import Builder.BuildBinary    as BuildBinary (binary)
import Builder.BuildFunction  as BuildFunction (funcEpilogue, funcPrologue)
import Builder.BuildState     (BuildState, runBuildState)
import Builder.BuildState     as BuildState (startState)
import Builder.BuildTernary   as BuildTernary (ternary)
import Builder.BuildUnary     as BuildUnary (unary)
import Builder.BuildVariables as BuildVariables (addressOf, loadLiteral,
                                                 loadVariable)
import Types.AssemblySchema
import Types.Error
import Types.Variables        (VarType (..))


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

buildASM DeclarationSchema{} = pure ""

buildASM (StatementSchema statement) = buildStatementASM statement

buildASM (ExpressionSchema expression) = buildExpressionASM expression

buildASM SkipSchema = pure ""



buildStatementASM :: StatementSchema -> BuildState String

buildStatementASM (ReturnSchema expression) = buildExpressionASM expression

buildStatementASM (CompoundStatementSchema items) = concatMapM buildASM items

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

buildExpressionASM _                 = pure ""
