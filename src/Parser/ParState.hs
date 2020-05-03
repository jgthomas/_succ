{-|
Module       : ParState
Description  : State for the parser

State holder for the parsing stage of compilation.
-}
module Parser.ParState
        (ParserState,
         runParState,
         throwError,
         getState,
         putState,
         startState
        ) where


import           Error.Error     (CompilerError (ImpossibleError))
import           Types.AST       (Tree (ProgramNode))
import           Types.SuccState (SuccStateM, throwError)
import qualified Types.SuccState as SuccState (getState, putState, runSuccState)


-- | State definition
type ParserState = SuccStateM Tree


-- | State constructor
startState :: Tree
startState = ProgramNode []


-- | Run the state extracting the error or result
runParState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runParState f t s = SuccState.runSuccState f t s


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
