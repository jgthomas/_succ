module Assembler.Assembler
  ( assemble,
  )
where

import System.Exit (ExitCode)
import System.FilePath (dropExtension)
import System.Process (system)

-- | Convert assembly into machine code
assemble :: FilePath -> IO ExitCode
assemble asmFileName = gccAssemble asmFileName

gccAssemble :: FilePath -> IO ExitCode
gccAssemble asmFileName = system $ assembler ++ opts ++ asmFileName ++ binaryOut
  where
    assembler = "gcc "
    opts = "-g "
    binaryOut = " -o " ++ dropExtension asmFileName
