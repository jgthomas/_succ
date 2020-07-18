
module Converter.Converter (convert) where


import           Control.Monad        (unless)

import           State.GenState       (GenState, runGenState, throwError)
import qualified State.GenState       as GenState (getState, startState)
import           State.SymTab         (SymTab)
import qualified State.SymTab         as SymTab
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

convertToSchema funcNode@(FunctionNode _ _ _ Nothing _) = do
        declareFunction funcNode
        pure SkipSchema
convertToSchema funcNode@(FunctionNode _ name _ (Just body) _) = do
        declareFunction funcNode
        SymTab.initFunction name
        bodySchema <- convertToSchema body
        SymTab.closeFunction
        SymTab.defineFunction name
        pure (FunctionSchema name bodySchema)

convertToSchema (ParamNode typ (VarNode name _) _) = do
        SymTab.addParameter name typ
        pure SkipSchema
convertToSchema node@ParamNode{} =
        throwError $ FatalError (GeneratorBug node)

convertToSchema (ArgNode arg _) = convertToSchema arg

convertToSchema (CompoundStmtNode statements _) = do
        SymTab.initScope
        statementsSchema <- mapM convertToSchema statements
        SymTab.closeScope
        pure (StatementSchema $ CompoundStatementSchema statementsSchema)

convertToSchema (ReturnNode val _) = do
        value <- getExpressionSchema <$> convertToSchema val
        pure (StatementSchema $ ReturnSchema value)

convertToSchema (UnaryNode val unOp _) = do
        value <- getExpressionSchema <$> convertToSchema val
        pure (ExpressionSchema $ UnarySchema value unOp)

convertToSchema (ConstantNode n _) = pure (ExpressionSchema $ LiteralSchema n)

convertToSchema (VarNode name _) = pure (ExpressionSchema $ VariableSchema name)

convertToSchema tree = throwError $ FatalError (GeneratorBug tree)


getExpressionSchema :: AssemblySchema -> ExpressionSchema
getExpressionSchema (ExpressionSchema schema) = schema
getExpressionSchema _                         = undefined



-- Function

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode _ funcName _ _ _) = do
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> declareNewFunction node
             Just _  -> declareRepeatFunction node
declareFunction tree = throwError $ FatalError (GeneratorBug tree)


declareNewFunction :: Tree -> GenState ()
declareNewFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        processParameters funcName paramList
declareNewFunction tree = throwError $ FatalError (GeneratorBug tree)


declareRepeatFunction :: Tree -> GenState ()
declareRepeatFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ FatalError (GeneratorBug tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ convertToSchema params
        SymTab.closeFunction
