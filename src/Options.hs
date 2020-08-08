{-# LANGUAGE DeriveDataTypeable #-}

module Options where


import System.Console.CmdArgs (Data, Typeable)

import Types.SuccTokens       (Debug (..), Optimise (..))


data SuccArgs = SuccArgs {
        debug :: Bool
      , stage :: String
      , file  :: FilePath
} deriving (Show, Data, Typeable)


data SuccOptions = SuccOptions {
        debugSet    :: Debug
      , optimiseSet :: Optimise
}


buildOptions :: SuccArgs -> SuccOptions
buildOptions args = SuccOptions {
        debugSet = debugStatus (debug args) (stage args),
        optimiseSet = OptimiseOn
}


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
