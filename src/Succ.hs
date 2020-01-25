{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           Data.Map    as M

import           System.Exit (exitFailure)

import qualified Checker     (check)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import qualified PrintError  (printError)


-- | Run the compilation process
compile :: String -> IO String
compile input = do
        toks <- errorHandler . Lexer.tokenize $ input
        ast  <- errorHandler . Parser.parse $ toks
        ast' <- errorHandler . Checker.check $ ast
        errorHandler . Generator.generate $ ast'
        where
                errorHandler = handleError $ toLineMap input


handleError :: M.Map Int String -> Either CompilerError a -> IO a
handleError _ (Right out) = pure out
handleError lineMap (Left err)  = do
        PrintError.printError lineMap err
        exitFailure


toLineMap :: String -> M.Map Int String
toLineMap input = M.fromList $ zip [1..] $ lines input
