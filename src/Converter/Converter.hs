
module Converter.Converter (convert) where


import           State.GenState       (GenState, runGenState, throwError)
import qualified State.GenState       as GenState (getState, startState)
import           State.SymTab         (SymTab)
import           Types.AssemblySchema
import           Types.AST            (Tree (..))
import           Types.Error          (CompilerError (FatalError),
                                       FatalError (GeneratorBug))


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

convertToSchema (FunctionNode _ name trees _ _) = do
        schemas <- mapM convertToSchema trees
        pure (FunctionSchema name schemas)

convertToSchema (ReturnNode val _) = do
        retVal <- convertToSchema val
        case retVal of
             (ExpressionSchema valSchema) ->
                     pure (StatementSchema $ ReturnSchema valSchema)
             _ -> undefined

convertToSchema (ConstantNode n _) = pure (ExpressionSchema $ Literal n)

convertToSchema (VarNode name _) = pure (ExpressionSchema $ Variable name)

convertToSchema tree = throwError $ FatalError (GeneratorBug tree)
