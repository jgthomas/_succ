
module Debug where


import AST      (Tree)
import GenState (SymTab)
import LexDat   (LexDat)


data Debug = DebugOn
           | DebugOff
           deriving (Eq)


debug :: Debug -> String -> [LexDat] -> Tree -> SymTab -> String -> IO ()
debug debugSet input lexed parsed symTab asm =
        case debugSet of
             DebugOff -> pure ()
             DebugOn  -> do
                     newLine
                     putStrLn "INPUT"
                     putStrLn input
                     putStrLn "AFTER LEXING"
                     newLine
                     print lexed
                     newLine
                     putStrLn "AFTER PARSING"
                     newLine
                     print parsed
                     newLine
                     putStrLn "STATE"
                     newLine
                     print symTab
                     newLine
                     putStrLn "OUTPUT"
                     newLine
                     putStrLn asm
                     newLine


newLine :: IO ()
newLine = putStr "\n"
