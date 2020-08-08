{-# LANGUAGE DeriveDataTypeable #-}

module Options (SuccArgs(..), SuccOptions(..), buildOptions) where


import System.Console.CmdArgs (Data, Typeable)

import Types.SuccTokens       (Debug (..), Optimise (..))


data SuccArgs = SuccArgs {
        debug    :: Bool
      , optimise :: Bool
      , stage    :: String
      , file     :: FilePath
} deriving (Show, Data, Typeable)


data SuccOptions = SuccOptions {
        debugSet    :: Debug
      , optimiseSet :: Optimise
}


buildOptions :: SuccArgs -> SuccOptions
buildOptions args = SuccOptions {
        debugSet    = debugStatus (debug args) (stage args)
      , optimiseSet = setOptimise (optimise args)
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
setDebugStatus _        = DebugOn
