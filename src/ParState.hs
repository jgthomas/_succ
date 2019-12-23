
module ParState
        (ParserState,
         runParState,
         throwError,
         getState,
         putState
        ) where


import           AST       (Tree (ProgramNode))
import           Error     (CompilerError (ImpossibleError))
import           SuccState (SuccStateM, throwError)
import qualified SuccState (getState, putState, runSuccState)


type ParserState = SuccStateM Tree


-- | Run the state extracting the error or result
runParState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runParState f t s = SuccState.runSuccState f t s


getState :: ParserState [Tree]
getState = do
        ast <- SuccState.getState
        getTreeList ast


putState :: s -> SuccStateM s ()
putState s = SuccState.putState s


getTreeList :: Tree -> ParserState [Tree]
getTreeList (ProgramNode treeList) = pure treeList
getTreeList _                      = throwError ImpossibleError
