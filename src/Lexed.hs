
module Lexed
        (Lexed(lineNum, lineMap, tokPos),
         mkLexed,
         tokenState,
         posState,
         getState,
         incLineNum,
         addToken
        ) where


import           Data.Map  as M

import           SuccState (SuccStateM)
import qualified SuccState (getState, putState)
import           Tokens    (Token)


type LexerState = SuccStateM Lexed


data Lexed = Lexed { lineNum :: Int
                   , lineMap :: M.Map Int String
                   , tokPos  :: [(Int, Token)] }
           deriving (Show)


mkLexed :: String -> Lexed
mkLexed cs = Lexed 0 (toLines cs) []


toLines :: String -> M.Map Int String
toLines cs = M.fromList $ zip [0..] $ lines cs


tokenState :: LexerState [Token]
tokenState = reverse . snd <$> splitState


posState :: LexerState [Int]
posState = reverse . fst <$> splitState


splitState :: LexerState ([Int], [Token])
splitState = unzip . tokPos <$> SuccState.getState


incLineNum :: LexerState ()
incLineNum = do
        state <- SuccState.getState
        SuccState.putState $ state { lineNum = succ . lineNum $ state }


addToken :: Token -> LexerState ()
addToken tok = do
        state <- SuccState.getState
        SuccState.putState $ state { tokPos = (lineNum state, tok):tokPos state }
        incLineNum


getState :: LexerState [(Int, Token)]
getState = tokPos <$> SuccState.getState
