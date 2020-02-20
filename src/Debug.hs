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
debug DebugOff _ _ _ _ _ = pure ()
debug DebugOn input lexed tree symTab asm = do
        debugInput input
        debugLexed lexed
        debugAst tree
        debugState symTab
        debugOutput asm


debugInput :: String -> IO ()
debugInput input = do
        putStrLn "\nC CODE"
        putStrLn input
        newLine


debugLexed :: [LexDat] -> IO ()
debugLexed lexed = do
        putStrLn "LEXED TOKENS\n"
        print lexed
        newLine


debugAst :: Tree -> IO ()
debugAst tree = do
        putStrLn "ABSTRACT SYNTAX TREE\n"
        pPrint tree
        newLine


debugState :: SymTab -> IO ()
debugState symTab = do
        putStrLn "STATE\n"
        pPrint symTab
        newLine


debugOutput :: String -> IO ()
debugOutput asm = do
        putStrLn "ASSEMBLY CODE\n"
        putStrLn asm
        newLine


newLine :: IO ()
newLine = putStr "\n"
