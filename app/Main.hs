
module Main (main) where


import Control.DeepSeq        (deepseq)
import System.Console.CmdArgs
import System.FilePath        (dropExtension)
import System.IO              (IOMode (ReadMode), hClose, hGetContents,
                               openFile, writeFile)
import System.Process         (system)

import Options                (SuccArgs (..), buildOptions)
import Succ                   (compile)


options :: SuccArgs
options = SuccArgs {
        debug    = False &= help "Display output of each compilation stage"
      , optimise = False &= help "Produce optimised assembly"
      , stage    = def &= typ "STAGE" &= help "Compilation stage to debug"
      , asmfile  = def &= typ "FILE" &= help "Outfile name"
      , file     = def &= argPos 0
} &= program "succ"


main :: IO ()
main = do
        arguments <- cmdArgs options

        let infileName     = file arguments
            outfileName    = setOutFile infileName (asmfile arguments)
            compileOptions = buildOptions arguments

        cFile <- openFile infileName ReadMode
        cCode <- hGetContents cFile
        asm   <- compile cCode compileOptions

        -- force evaluation before writing to file
        asm `deepseq` writeFile outfileName asm

        let gccOpts = "gcc -g "
            output  = " -o " ++ dropExtension outfileName
            toMachineCode = gccOpts ++ outfileName ++ output
            deleteFile    = "rm " ++ outfileName

        _ <- system toMachineCode
        _ <- system deleteFile
        hClose cFile


setOutFile :: FilePath -> FilePath -> FilePath
setOutFile infile ""       = dropExtension infile ++ ".s"
setOutFile _      filename = filename ++ ".s"
