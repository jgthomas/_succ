
module Validate where


import           Control.Monad (when)

import           AST           (Tree (..))
import           Error         (CompilerError (SyntaxError), SyntaxError (..))
import           GenState      (GenState, throwError)
import qualified SymTab


checkIfUsedInScope :: Tree -> GenState ()
checkIfUsedInScope node@(DeclarationNode name _ _) = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           throwError $ SyntaxError (DoubleDeclared node)
checkIfUsedInScope tree = throwError $ SyntaxError (Unexpected tree)
