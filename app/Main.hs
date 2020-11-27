module Main
  ( main,
  )
where

import qualified Assembler.Assembler as Assembler (assemble)
import qualified Options
  ( SuccArgs (..),
    buildCompilerOptions,
    options,
    setFileNames,
  )
import qualified Succ (compile)
import System.Console.CmdArgs (cmdArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import System.Process (system)
import Control.Monad (unless)

main :: IO ()
main = do
  arguments <- cmdArgs Options.options
  let (infileName, outfileName) = Options.setFileNames arguments
      compileOptions = Options.buildCompilerOptions arguments
  cFile <- openFile infileName ReadMode
  cCode <- hGetContents cFile
  asm <- Succ.compile cCode compileOptions
  writeFile outfileName asm
  _ <- Assembler.assemble outfileName
  unless (Options.keep arguments) $ do
    _ <- system $ "rm " ++ outfileName
    hClose cFile
  hClose cFile
