{-|
Module       : LexState
Description  : State for the lexer

State holder for the lexing stage of compilation.
-}
module Lexer.LexState
        (LexerState,
         runLexState,
         throwError,
         getState,
         startState,
         addToken,
         incLineNum
        ) where


import           Error.Error     (CompilerError)
import           Lexer.LexTab    (LexDat, LexTab (..))
import qualified Lexer.LexTab    as LexTab (mkLexDat, mkLexTab)
import           Types.SuccState (SuccStateM, throwError)
import qualified Types.SuccState as SuccState (getState, putState, runSuccState)
import           Types.Tokens    (Token)


-- | State definition
type LexerState = SuccStateM LexTab


-- | Initial state
startState :: LexTab
startState = LexTab.mkLexTab


-- | Increment the line number in the state
incLineNum :: LexerState ()
incLineNum = do
        state <- SuccState.getState
        SuccState.putState $ state { lineNum = succ . lineNum $ state }


-- | Build LexDat from token and add to state
addToken :: Token -> LexerState ()
addToken tok = do
        lexDat <- mkLexDat tok
        state  <- SuccState.getState
        SuccState.putState $ state { datList = lexDat:datList state }


mkLexDat :: Token -> LexerState LexDat
mkLexDat tok = do
        lineN <- lineNum <$> SuccState.getState
        pure $ LexTab.mkLexDat tok lineN


-- | Return the list of LexDat from the state
getState :: LexerState [LexDat]
getState = datList <$> SuccState.getState


-- | Run the state extracting the error or result
runLexState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runLexState f t s = SuccState.runSuccState f t s
