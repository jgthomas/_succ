
module Checker (check) where


import           Control.Monad (when)
import           Data.Maybe    (isNothing)

import           AST           (Tree (..))
import           Error         (CompilerError (SyntaxError), SyntaxError (..))
import           GenState      (GenState, runGenState, throwError)
import qualified GenState      (startState)
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

checkAST ForLoopNode{} = do
        SymTab.initScope
        _         <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue contLabel
        SymTab.closeScope

checkAST WhileNode{} = do
        loopLabel <- SymTab.labelNum
        testLabel <- SymTab.labelNum
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel

checkAST DoWhileNode{} = do
        _         <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel

checkAST (IfNode _ _ possElse) = do
        _ <- SymTab.labelNum
        case possElse of
             Nothing -> pure ()
             Just _  -> do
                     _ <- SymTab.labelNum
                     pure ()

checkAST ContinueNode = do
        continueLabel <- SymTab.getContinue
        when (isNothing continueLabel) $
            throwError $ SyntaxError (Unexpected ContinueNode)

checkAST BreakNode = do
        breakLabel <- SymTab.getContinue
        when (isNothing breakLabel) $
            throwError $ SyntaxError (Unexpected BreakNode)

checkAST _ = pure ()
