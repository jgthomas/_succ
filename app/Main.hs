
module Main (main) where


import Control.DeepSeq    (deepseq)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    (dropExtension)
import System.IO          (IOMode (ReadMode), hClose, hGetContents, openFile,
                           writeFile)
import System.Process     (system)

import Succ               (compile)


main :: IO ()
main = do
        args <- getArgs

        (infileName, debugSetting) <- checkInput args
        let outfileName = dropExtension infileName ++ ".s"

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


checkInput :: [String] -> IO (String, Maybe String)
checkInput (x:y:_) = pure (x, Just y)
checkInput (x:_)   = pure (x, Nothing)
checkInput []      = do
        putStrLn "No input file provided"
        exitFailure
