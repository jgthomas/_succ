
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
getInput args =
        let (file, debug) = checkInput args
            in
        case (file, debug) of
             (Nothing, _) -> do
                     putStrLn "No input file provided"
                     exitFailure
             (Just x, Nothing) -> pure (x, DebugOff)
             (Just x, Just y) ->
                     if y == "debug"
                        then pure (x, DebugOn)
                        else pure (x, DebugOff)


checkInput :: [String] -> (Maybe String, Maybe String)
checkInput []      = (Nothing, Nothing)
checkInput (x:y:_) = (Just x, Just y)
checkInput (x:_)   = (Just x, Nothing)
