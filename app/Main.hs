module Main
  ( main,
  )
where

import Control.DeepSeq (deepseq)
import qualified Options
  ( buildCompilerOptions,
    options,
    setFileNames,
  )
import qualified Succ (compile)
import System.Console.CmdArgs (cmdArgs)
import System.FilePath (dropExtension)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import System.Process (system)

main :: IO ()
main = do
  arguments <- cmdArgs Options.options
  let (infileName, outfileName) = Options.setFileNames arguments
      compileOptions = Options.buildCompilerOptions arguments
  cFile <- openFile infileName ReadMode
  cCode <- hGetContents cFile
  asm <- Succ.compile cCode compileOptions
  -- force evaluation before writing to file
  asm `deepseq` writeFile outfileName asm
  let gccOpts = "gcc -g "
      output = " -o " ++ dropExtension outfileName
      toMachineCode = gccOpts ++ outfileName ++ output
      deleteFile = "rm " ++ outfileName
  _ <- system toMachineCode
  _ <- system deleteFile
  hClose cFile
