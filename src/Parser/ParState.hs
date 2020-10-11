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


import Types.AST       (Tree)
import Types.SuccState (SuccStateM, evaluate, getState, putState, throwError)


-- | State definition
type ParserState = SuccStateM [Tree]


-- | State constructor
startState :: [Tree]
startState = []
