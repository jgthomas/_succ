{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import qualified Checker.Checker       as Checker (check)
import qualified Debug.Debug           as Debug (debug, setDebugLevel)
import qualified Generator.Generator   as Generator (generate)
import qualified Lexer.Lexer           as Lexer (tokenize)
import qualified Parser.Parser         as Parser (parse)
import qualified PrintError.PrintError as PrintError (handleError)
import           Types.SuccTokens      (Debug, Stage (..))


-- | Run the compilation process
compile :: String -> Maybe String -> IO String
compile input debugSet = do
        input' <- debugInput . pure $ input
        toks   <- debugLexer . errorHandler . Lexer.tokenize $ input'
        ast    <- debugParser . errorHandler . Parser.parse $ toks
        ast'   <- errorHandler . Checker.check $ ast
        fmap fst . debugOutput . errorHandler . Generator.generate $ ast'
        where
                debugLevel   = Debug.setDebugLevel debugSet
                debugInput   = Debug.debug debugLevel Input
                debugLexer   = Debug.debug debugLevel Lexer
                debugParser  = Debug.debug debugLevel Parser
                debugOutput  = debugOut debugLevel
                errorHandler = PrintError.handleError debugLevel input


debugOut :: (Show a, Show b) => Debug -> IO (a, b) -> IO (a, b)
debugOut debugLevel output = do
        (asm, symTab) <- output
        _ <- Debug.debug debugLevel State (pure symTab)
        _ <- Debug.debug debugLevel Output (pure asm)
        output
