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
import GenState           (SymTab)
import LexDat             (LexDat)


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
                     putStrLn "INPUT C CODE"
                     putStrLn input
                     putStrLn "LEXED TOKENS"
                     newLine
                     print lexed
                     newLine
                     putStrLn "ABSTRACT SYNTAX TREE"
                     newLine
                     pPrint parsed
                     newLine
                     putStrLn "STATE"
                     newLine
                     print symTab
                     newLine
                     putStrLn "OUTPUT ASSEMBLY CODE"
                     newLine
                     putStrLn asm
                     newLine


newLine :: IO ()
newLine = putStr "\n"
