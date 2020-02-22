{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit (exitFailure)

import qualified Checker     (check)
import           Debug       (Debug (..), Stage (..), debugs)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import qualified PrintError  (printError)


-- | Run the compilation process
compile :: String -> Maybe String -> IO String
compile input debugSet = do
        _ <- debugs Input (pure input)
        toks          <- (debugs Lexer) . errorHandler . Lexer.tokenize $ input
        ast           <- (debugs Parser) . errorHandler . Parser.parse $ toks
        ast'          <- errorHandler . Checker.check $ ast
        (asm, symTab) <- errorHandler . Generator.generate $ ast'
        --Debug.debug debugLevel input toks ast symTab asm
        _ <- debugs State (pure symTab)
        _ <- debugs Output (pure asm)
        pure asm
        where
                debugLevel   = setDebugLevel debugSet
                errorHandler = handleError debugLevel input


handleError :: Debug -> String -> Either CompilerError a -> IO a
handleError _ _ (Right out) = pure out
handleError debugSet input (Left err)  = do
        PrintError.printError debugSet input err
        exitFailure


setDebugLevel :: Maybe String -> Debug
setDebugLevel Nothing = DebugOff
setDebugLevel (Just debug)
        | debug == "debug" = DebugOn
        | otherwise        = DebugOff
