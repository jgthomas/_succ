module Assembler.Assembler
  ( assemble,
  )
where

import System.Exit (ExitCode)
import System.FilePath (dropExtension)
import System.Process (system)

assemble :: FilePath -> IO ExitCode
assemble asmFileName = system $ assembler ++ opts ++ asmFileName ++ binaryOut
  where
    assembler = "gcc "
    opts = "-g "
    binaryOut = " -o " ++ dropExtension asmFileName
