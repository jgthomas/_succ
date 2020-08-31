{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import qualified Builder.Builder       as Builder (build)
--import qualified Checker.Checker       as Checker (check)
import qualified Converter.Converter   as Converter (convert)
import qualified Debug.Debug           as Debug (debug, debugPair)
import qualified Lexer.Lexer           as Lexer (tokenize)
import           Options               (SuccOptions (..))
import qualified Parser.Parser         as Parser (parse)
import qualified PrintError.PrintError as PrintError (handleError)
import           Types.SuccTokens      (Stage (..))


-- | Run the compilation process
compile :: String -> SuccOptions -> IO String
compile input options = do
        input' <- debugInput . pure $ input
        toks   <- debugLexer . errorHandler . Lexer.tokenize $ input'
        ast    <- debugParser . errorHandler . Parser.parse $ toks
        --ast'   <- debugChecker . errorHandler . Checker.check $ ast
        schema <- fmap fst . debugSchema . errorHandler . Converter.convert $ ast
        debugAssembly . errorHandler . Builder.build optimiseFlag $ schema
        where
                debugFlag     = debugSet options
                optimiseFlag  = optimiseSet options
                debugInput    = Debug.debug debugFlag Input
                debugLexer    = Debug.debug debugFlag Lexer
                debugParser   = Debug.debug debugFlag Parser
                --debugChecker  = Debug.debug debugFlag Check
                debugSchema   = Debug.debugPair debugFlag (Schema, State)
                debugAssembly = Debug.debug debugFlag Output
                errorHandler  = PrintError.handleError debugFlag input
