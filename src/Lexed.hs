
module Lexed
        (lineMap,
         mkLexerState,
         tokState,
         posState,
         incLineNum,
         addToken,
         runSuccState,
         throwError,
         runLexState,
         getState,
         Lexed,
         LexerState
        ) where


import           Data.Map  as M

import           Error     (CompilerError)
import           SuccState (SuccStateM, runSuccState, throwError)
import qualified SuccState (getState, putState)
import           Tokens    (Token)


type LexerState = SuccStateM Lexed


data Lexed = Lexed { lineNum :: Int
                   , lineMap :: M.Map Int String
                   , tokList :: [Token]
                   , posList :: [Int] }
           deriving (Show)


mkLexerState :: String -> Lexed
mkLexerState cs = Lexed 0 (toLines cs) [] []


toLines :: String -> M.Map Int String
toLines cs = M.fromList $ zip [0..] $ lines cs


incLineNum :: LexerState ()
incLineNum = do
        state <- SuccState.getState
        SuccState.putState $ state { lineNum = succ . lineNum $ state }


addToken :: Token -> LexerState ()
addToken tok = do
        state <- SuccState.getState
        let state'  = state { tokList = tok:tokList state }
            state'' = state' { posList = lineNum state':posList state' }
        SuccState.putState state''


tokState :: LexerState [Token]
tokState = getPartState tokList


posState :: LexerState [Int]
posState = getPartState posList


getPartState :: (Lexed -> [a]) -> LexerState [a]
getPartState f = do
        state <- SuccState.getState
        pure . reverse . f $ state


getState :: LexerState Lexed
getState = SuccState.getState


runLexState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runLexState f t s = SuccState.runSuccState f t s
