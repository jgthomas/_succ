{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug
        (Debug(..),
         debug
        ) where


import Text.Pretty.Simple (pPrint)

import AST                (Tree)
import LexTab             (LexDat)
import SymbolTable        (SymTab)


-- | Debug switch definition
data Debug = DebugOn
           | DebugOff
           deriving (Eq)


-- | Output debugging information
debug :: Debug -> String -> [LexDat] -> Tree -> SymTab -> String -> IO ()
debug debugSet input lexed parsed symTab asm =
        case debugSet of
             DebugOff -> pure ()
             DebugOn  -> do
                     newLine
                     debugInput input
                     newLine
                     debugLexed lexed
                     newLine
                     debugAst parsed
                     newLine
                     debugState symTab
                     newLine
                     debugOutput asm
                     newLine


debugInput :: String -> IO ()
debugInput input = do
        putStrLn "INPUT C CODE"
        putStrLn input


debugLexed :: [LexDat] -> IO ()
debugLexed lexed = do
        putStrLn "LEXED TOKENS\n"
        print lexed


debugAst :: Tree -> IO ()
debugAst tree = do
        putStrLn "ABSTRACT SYNTAX TREE\n"
        pPrint tree


debugState :: SymTab -> IO ()
debugState symTab = do
        putStrLn "STATE\n"
        pPrint symTab


debugOutput :: String -> IO ()
debugOutput asm = do
        putStrLn "OUTPUT ASSEMBLY CODE\n"
        putStrLn asm


newLine :: IO ()
newLine = putStr "\n"
