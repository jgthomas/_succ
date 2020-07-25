
module Builder.Builder (build) where


import Control.Monad.Extra    (concatMapM)

import Builder.BuildFunction  (funcEpilogue, funcPrologue)
import Builder.BuildState     (BuildState, runBuildState)
import Builder.BuildState     as BuildState (startState)
import Builder.BuildVariables (loadLiteral)
import Types.AssemblySchema
import Types.Error


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
                prologue = funcPrologue name
                epilogue = funcEpilogue

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

buildExpressionASM _                 = pure ""
