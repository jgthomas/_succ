{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import qualified Checker.Checker       as Checker (check)
import qualified Converter.Converter   as Converter (convert)
import qualified Debug.Debug           as Debug (debug, debugPair)
import qualified Generator.Generator   as Generator (generate)
import qualified Lexer.Lexer           as Lexer (tokenize)
import qualified Parser.Parser         as Parser (parse)
import qualified PrintError.PrintError as PrintError (handleError)
import           Types.SuccTokens      (Stage (..))


-- | Run the compilation process
compile :: String -> Maybe String -> IO String
compile input debugOption = do
        input' <- debugInput . pure $ input
        toks   <- debugLexer . errorHandler . Lexer.tokenize $ input'
        ast    <- debugParser . errorHandler . Parser.parse $ toks
        ast'   <- debugChecker . errorHandler . Checker.check $ ast
        _      <- fmap fst . debugSchema . errorHandler . Converter.convert $ ast'
        fmap fst . debugOutput . errorHandler . Generator.generate $ ast'
        where
                debugInput   = Debug.debug debugOption Input
                debugLexer   = Debug.debug debugOption Lexer
                debugParser  = Debug.debug debugOption Parser
                debugChecker = Debug.debug debugOption Check
                debugSchema  = Debug.debugPair debugOption (Schema, State)
                debugOutput  = Debug.debugPair debugOption (Output, State)
                errorHandler = PrintError.handleError debugOption input
