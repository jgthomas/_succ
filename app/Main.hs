{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where


import Control.DeepSeq        (deepseq)
import System.Console.CmdArgs
import System.FilePath        (dropExtension)
import System.IO              (IOMode (ReadMode), hClose, hGetContents,
                               openFile, writeFile)
import System.Process         (system)

import Succ                   (compile)


data Succ = Succ {
        debug :: Bool
      , file  :: FilePath
} deriving (Show, Data, Typeable)


options :: Succ
options = Succ {
        debug = False &= help "Display output of each compilation stage"
      , file  = def &= argPos 0
}


main :: IO ()
main = do
        arguments <- cmdArgs options

        let infileName   = file arguments
            outfileName  = dropExtension infileName ++ ".s"
            debugSetting = if debug arguments
                              then (Just "debug")
                              else Nothing

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
