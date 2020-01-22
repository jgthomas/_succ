
module Lexed
        (lineMap,
         mkLexed,
         tokState,
         posState,
         incLineNum,
         addToken,
         runSuccState,
         throwError,
         runLexState
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


mkLexed :: String -> Lexed
mkLexed cs = Lexed 0 (toLines cs) [] []


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
tokState = getState tokList


posState :: LexerState [Int]
posState = getState posList


getState :: (Lexed -> [a]) -> LexerState [a]
getState f = do
        state <- SuccState.getState
        pure . reverse . f $ state


runLexState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runLexState f t s = SuccState.runSuccState f t s
