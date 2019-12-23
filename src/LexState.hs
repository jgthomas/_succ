{-|
Module       : LexState
Description  : State for the lexer

State holder for the lexing stage of compilation.
-}
module LexState
        (LexerState,
         runLexState,
         throwError,
         getState,
         putState,
         startState
        ) where


import           Error     (CompilerError)
import           SuccState (SuccStateM, getState, putState, throwError)
import qualified SuccState (runSuccState)
import           Tokens    (Token)


-- | State definition
type LexerState = SuccStateM [Token]


-- | Initial state
startState :: [Token]
startState = []


-- | Run the state extracting the error or result
runLexState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runLexState f t s = SuccState.runSuccState f t s
