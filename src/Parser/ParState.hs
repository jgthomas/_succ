{-|
Module       : ParState
Description  : State for the parser

State holder for the parsing stage of compilation.
-}
module Parser.ParState
        (ParserState,
         evaluate,
         throwError,
         getState,
         putState,
         startState
        ) where


import           Types.AST       (Tree (ProgramNode))
import           Types.Error     (CompilerError (ImpossibleError))
import           Types.SuccState (SuccStateM, evaluate, throwError)
import qualified Types.SuccState as SuccState (getState, putState)


-- | State definition
type ParserState = SuccStateM Tree


-- | State constructor
startState :: Tree
startState = ProgramNode []


-- | Get the state
getState :: ParserState [Tree]
getState = do
        ast <- SuccState.getState
        getTreeList ast


-- | Update the state
putState :: s -> SuccStateM s ()
putState s = SuccState.putState s


getTreeList :: Tree -> ParserState [Tree]
getTreeList (ProgramNode treeList) = pure treeList
getTreeList _                      = throwError ImpossibleError
