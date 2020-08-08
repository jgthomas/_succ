{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where


import Control.DeepSeq        (deepseq)
import System.Console.CmdArgs
import System.FilePath        (dropExtension)
import System.IO              (IOMode (ReadMode), hClose, hGetContents,
                               openFile, writeFile)
import System.Process         (system)

import Succ                   (compile)
import Types.SuccTokens       (Debug (..))


data Succ = Succ {
        debug :: Bool
      , stage :: String
      , file  :: FilePath
} deriving (Show, Data, Typeable)


options :: Succ
options = Succ {
        debug = False &= help "Display output of each compilation stage"
      , stage = "all" &= typ "STAGE" &= help "Compilation stage to debug"
      , file  = def &= argPos 0
}


main :: IO ()
main = do
        arguments <- cmdArgs options

        let infileName   = file arguments
            outfileName  = dropExtension infileName ++ ".s"
            debugSetting = debugStatus (debug arguments) (stage arguments)

        cFile <- openFile infileName ReadMode
        cCode <- hGetContents cFile
        asm   <- compile cCode debugSetting

        -- force evaluation before writing to file
        asm `deepseq` writeFile outfileName asm

        let gccOpts = "gcc -g "
            output  = " -o " ++ dropExtension outfileName
            toMachineCode = gccOpts ++ outfileName ++ output
            deleteFile    = "rm " ++ outfileName

        _ <- system toMachineCode
        _ <- system deleteFile
        hClose cFile


debugStatus :: Bool -> String -> Debug
debugStatus False _         = DebugOff
debugStatus True debugStage = setDebugStatus debugStage


setDebugStatus :: String -> Debug
setDebugStatus "all"    = DebugOn
setDebugStatus "lexer"  = DebugLexer
setDebugStatus "parser" = DebugParser
setDebugStatus "schema" = DebugSchema
setDebugStatus "state"  = DebugState
setDebugStatus "asm"    = DebugAsm
setDebugStatus "code"   = DebugCode
setDebugStatus _        = DebugOff
