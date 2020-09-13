{-# LANGUAGE DeriveDataTypeable #-}

module Options
        (options,
         setFileNames,
         SuccOptions(..),
         buildCompilerOptions
        ) where


import System.Console.CmdArgs
import System.FilePath        (dropExtension)

import Types.SuccTokens       (Debug (..), Optimise (..))


data SuccArgs = SuccArgs {
        debug    :: Bool
      , optimise :: Bool
      , stage    :: String
      , asmfile  :: String
      , file     :: FilePath
} deriving (Show, Data, Typeable)


-- | Command line option definition
options :: SuccArgs
options = SuccArgs {
        debug    = False &= help "Display output of each compilation stage"
      , optimise = False &= help "Produce optimised assembly"
      , stage    = def &= typ "STAGE" &= help "Compilation stage to debug"
      , asmfile  = def &= typ "FILE" &= help "Outfile name"
      , file     = def &= argPos 0
} &= program "succ"


-- | Set the input and output file names
setFileNames :: SuccArgs -> (FilePath, FilePath)
setFileNames arguments = (infileName, outfileName)
        where
                infileName  = file arguments
                outfileName = setOutFile infileName (asmfile arguments)


setOutFile :: FilePath -> FilePath -> FilePath
setOutFile infile ""       = dropExtension infile ++ ".s"
setOutFile _      filename = filename ++ ".s"


-- | Compiler options data type
data SuccOptions = SuccOptions {
        debugSet    :: Debug
      , optimiseSet :: Optimise
}


-- | Build compiler options data container
buildCompilerOptions :: SuccArgs -> SuccOptions
buildCompilerOptions arguments = SuccOptions {
        debugSet    = debugStatus (debug arguments) (stage arguments)
      , optimiseSet = setOptimise (optimise arguments)
}


setOptimise :: Bool -> Optimise
setOptimise True  = OptimiseOn
setOptimise False = OptimiseOff


debugStatus :: Bool -> String -> Debug
debugStatus False _         = DebugOff
debugStatus True debugStage = setDebugStatus debugStage


setDebugStatus :: String -> Debug
setDebugStatus "lexer"  = DebugLexer
setDebugStatus "parser" = DebugParser
setDebugStatus "schema" = DebugSchema
setDebugStatus "state"  = DebugState
setDebugStatus "asm"    = DebugAsm
setDebugStatus "code"   = DebugCode
setDebugStatus "trees"  = DebugTrees
setDebugStatus _        = DebugOn
