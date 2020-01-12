
module Checker (check) where


import           AST      (Tree (..))
import           Error    (CompilerError)
import           GenState (GenState, runGenState)
import qualified GenState (startState)
import qualified SymTab


check :: Tree -> Either CompilerError Tree
check ast = runGenState checker ast GenState.startState


checker :: Tree -> GenState Tree
checker ast = do
        checkAST ast
        pure ast


checkAST :: Tree -> GenState ()

checkAST (ProgramNode topLevelItems) = mapM_ checkAST topLevelItems

checkAST (ArgNode arg) = checkAST arg

checkAST (CompoundStmtNode blockItems) = do
        SymTab.initScope
        mapM_ checkAST blockItems
        SymTab.closeScope

checkAST _ = pure ()
