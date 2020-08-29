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
import           Types.SuccState (SuccStateM, evaluate, putState, throwError)
import qualified Types.SuccState as SuccState (getState)


-- | State definition
type ParserState = SuccStateM Tree


-- | State constructor
startState :: Tree
startState = ProgramNode []


-- | Get the state
getState :: ParserState [Tree]
getState = do
        ast <- SuccState.getState
        case ast of
             (ProgramNode treeList) -> pure treeList
             _                      -> pure []
