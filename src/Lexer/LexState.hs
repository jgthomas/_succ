{-|
Module       : LexState
Description  : State for the lexer

State holder for the lexing stage of compilation.
-}
module Lexer.LexState
        (LexerState,
         evaluate,
         throwError,
         getState,
         startState,
         addToken,
         incLineNum,
         mkLexDat
        ) where


import           Types.SuccState (SuccStateM, evaluate, throwError)
import qualified Types.SuccState as SuccState (getState, putState)
import           Types.Tokens    (LexDat (LexDat), Token)


data LexTab = LexTab { datList :: [Token]
                     , lineNum :: Int }


-- | State definition
type LexerState = SuccStateM LexTab


-- | Initial state
startState :: LexTab
startState = LexTab [] 1


-- | Increment the line number in the state
incLineNum :: LexerState ()
incLineNum = do
        state <- SuccState.getState
        SuccState.putState $ state { lineNum = succ . lineNum $ state }


-- | Build LexDat from token and add to state
addToken :: Token -> LexerState ()
addToken tok = do
        --lexDat <- mkLexDat tok
        state  <- SuccState.getState
        SuccState.putState $ state { datList = tok:datList state }


mkLexDat :: String -> LexerState LexDat
mkLexDat input = do
        lineN <- lineNum <$> SuccState.getState
        pure $ LexDat input lineN


-- | Return the list of LexDat from the state
getState :: LexerState [Token]
getState = datList <$> SuccState.getState
