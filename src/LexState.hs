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
         startState,
         addToken,
         incLineNum
        ) where


import           Error     (CompilerError)
import           LexTab    (LexDat, LexTab (..))
import qualified LexTab    (mkLexDat, mkLexTab)
import           SuccState (SuccStateM, throwError)
import qualified SuccState (getState, putState, runSuccState)
import           Tokens    (Token)


-- | State definition
type LexerState = SuccStateM LexTab


-- | Initial state
startState :: LexTab
startState = LexTab.mkLexTab


incLineNum :: LexerState ()
incLineNum = do
        state <- SuccState.getState
        SuccState.putState $ state { lineNum = succ . lineNum $ state }


addToken :: Token -> LexerState ()
addToken tok = do
        lexDat <- mkLexDat tok
        state  <- SuccState.getState
        SuccState.putState $ state { datList = lexDat:datList state }


mkLexDat :: Token -> LexerState LexDat
mkLexDat tok = do
        lineN <- lineNum <$> SuccState.getState
        pure $ LexTab.mkLexDat tok lineN


getState :: LexerState [LexDat]
getState = datList <$> SuccState.getState


-- | Run the state extracting the error or result
runLexState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runLexState f t s = SuccState.runSuccState f t s
