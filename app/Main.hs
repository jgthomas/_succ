
module Main (main) where


import Control.DeepSeq    (deepseq)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    (dropExtension)
import System.IO          (IOMode (ReadMode), hClose, hGetContents, openFile,
                           writeFile)
import System.Process     (system)

import Debug              (Debug (..))
import Succ               (compile)


main :: IO ()
main = do
        args <- getArgs

        (infileName, debugSetting) <- getInput args
        let outfileName = dropExtension infileName ++ ".s"

        cFile <- openFile infileName ReadMode
        cCode <- hGetContents cFile
        asm   <- compile debugSetting cCode

        -- force evaluation before writing to file
        asm `deepseq` writeFile outfileName asm

        let gccOpts = "gcc -g "
            output  = " -o " ++ dropExtension outfileName
            toMachineCode = gccOpts ++ outfileName ++ output
            deleteFile    = "rm " ++ outfileName

        _ <- system toMachineCode
        _ <- system deleteFile
        hClose cFile


getInput :: [String] -> IO (String, Debug)
getInput args = checkInput args >>= setDebugLevel


checkInput :: [String] -> IO (Maybe String, Maybe String)
checkInput []      = pure (Nothing, Nothing)
checkInput (x:y:_) = pure (Just x, Just y)
checkInput (x:_)   = pure (Just x, Nothing)


setDebugLevel :: (Maybe String, Maybe String) -> IO (String, Debug)
setDebugLevel (Just x, Nothing) = pure (x, DebugOff)
setDebugLevel (Just x, Just y)
        | y == "debug" = pure (x, DebugOn)
        | otherwise    = pure (x, DebugOff)
setDebugLevel (Nothing, _) = do
        putStrLn "No input file provided"
        exitFailure
