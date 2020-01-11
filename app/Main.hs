
module Main where


import Control.DeepSeq    (deepseq)
import System.Environment (getArgs)
import System.FilePath    (dropExtension)
import System.IO          (IOMode (ReadMode), hClose, hGetContents, openFile,
                           writeFile)
import System.Process     (system)

import Runner             (compile)


main :: IO ()
main = do
        args <- getArgs
        let infileName = head args
        handle   <- openFile infileName ReadMode
        contents <- hGetContents handle

        asm <- compile contents

        let outfileName = dropExtension infileName ++ ".s"

        asm `deepseq` writeFile outfileName asm

        let gccOpts = "gcc -g "
            output  = " -o " ++ dropExtension outfileName
            toMachineCode = gccOpts ++ outfileName ++ output
            deleteFile    = "rm " ++ outfileName

        _ <- system toMachineCode
        _ <- system deleteFile
        hClose handle

