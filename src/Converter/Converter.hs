
module Converter.Converter (convert, ExpressionData(..)) where


import           State.GenState  (GenState, runGenState, throwError)
import qualified State.GenState  as GenState (getState, startState)
import           State.SymTab    (SymTab)
import qualified State.SymTab    as SymTab
import           Types.AST       (Tree (..))
import           Types.Error     (CompilerError (FatalError),
                                  FatalError (GeneratorBug))
import           Types.Variables (Scope (..), VarLookup (..), VarType (..))


data ExpressionData = Literal Int Scope
                    | Variable VarType


convert :: Tree -> Either CompilerError (ExpressionData, SymTab)
convert ast = runGenState convertWithState ast GenState.startState


convertWithState :: Tree -> GenState (ExpressionData, SymTab)
convertWithState ast = do
        schema <- convertToSchema ast
        symTab <- GenState.getState
        pure (schema, symTab)


convertToSchema :: Tree -> GenState ExpressionData

convertToSchema (ConstantNode n _) = Literal n <$> SymTab.getScope

convertToSchema node@(VarNode name _) = do
        var <- SymTab.getVariable name
        case var of
             NotFound  -> throwError $ FatalError (GeneratorBug node)
             VarType a -> pure (Variable a)

convertToSchema tree = throwError $ FatalError (GeneratorBug tree)
