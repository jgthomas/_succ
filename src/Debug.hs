{-|
Module       : Debug
Description  : Output debugging information

Internal debugger that outputs the results of each stage of
the compilation process undertaken by succ.
-}
module Debug
        (Debug(..),
         debug,
         debugs,
         Stage(..)
        ) where


import Text.Pretty.Simple

import AST                (Tree)
import LexTab             (LexDat)
import SymbolTable        (SymTab)


-- | Debug switch definition
data Debug = DebugOn
           | DebugOff
           deriving (Eq)


data Stage = Input
           | Lexer
           | Parser
           | State
           | Output
           deriving (Eq)


debugs :: Show a => Stage -> IO a -> IO a
debugs stage x = do
        y <- x
        case stage of
             Input  -> debugString inputTitle y
             Lexer  -> debugDataTypeSimple lexTitle y
             Parser -> debugDataType parTitle y
             State  -> debugDataType stateTitle y
             Output -> debugString outTitle y
        id x
        where
                inputTitle = "C CODE"
                lexTitle   = "LEXED TOKENS"
                parTitle   = "ABSTRACT SYNTAX TREE"
                stateTitle = "SYMBOL TABLE"
                outTitle   = "ASSEMBLY CODE"



debugString :: Show a => String -> a -> IO ()
debugString title content = do
        putStrLn title
        pPrintOpt options $ content
        putStr "\n"
        where
                options = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 0}


debugDataType :: Show a => String -> a -> IO ()
debugDataType title dat = do
        putStrLn title
        pPrint dat
        putStr "\n"


debugDataTypeSimple :: Show a => String -> a -> IO ()
debugDataTypeSimple title dat = do
        putStrLn title
        print dat
        putStr "\n"


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
