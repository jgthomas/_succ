{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit   (exitFailure)

import qualified Checker       (check)
import qualified Debug         (debug)
import           Error         (CompilerError)
import qualified Generator     (generate)
import qualified Lexer         (tokenize)
import qualified Parser.Parser as Parser (parse)
import qualified PrintError    (printError)
import           SuccTokens    (Debug (..), Stage (..))


-- | Run the compilation process
compile :: String -> Maybe String -> IO String
compile input debugSet = do
        input' <- debugInput . pure $ input
        toks   <- debugLexer . errorHandler . Lexer.tokenize $ input'
        ast    <- debugParser . errorHandler . Parser.parse $ toks
        ast'   <- errorHandler . Checker.check $ ast
        fmap fst . debugOutput . errorHandler . Generator.generate $ ast'
        where
                debugLevel   = setDebugLevel debugSet
                debugInput   = Debug.debug debugLevel Input
                debugLexer   = Debug.debug debugLevel Lexer
                debugParser  = Debug.debug debugLevel Parser
                debugOutput  = debugOut debugLevel
                errorHandler = handleError debugLevel input


handleError :: Debug -> String -> Either CompilerError a -> IO a
handleError _ _ (Right out) = pure out
handleError debugSet input (Left err)  = do
        PrintError.printError debugSet input err
        exitFailure


setDebugLevel :: Maybe String -> Debug
setDebugLevel Nothing = DebugOff
setDebugLevel (Just dbug)
        | dbug == "debug" = DebugOn
        | otherwise       = DebugOff


debugOut :: (Show a, Show b) => Debug -> IO (a, b) -> IO (a, b)
debugOut debugLevel out = do
        (asm, symTab) <- out
        _ <- debugState (pure symTab)
        _ <- debugOutput (pure asm)
        out
        where
                debugState  = Debug.debug debugLevel State
                debugOutput = Debug.debug debugLevel Output


