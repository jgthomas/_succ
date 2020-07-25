
module Builder.Builder (build) where


import Control.Monad.Extra   (concatMapM)

import Builder.BuildFunction (funcEpilogue, funcPrologue)
import Builder.BuildState    (BuildState, runBuildState)
import Builder.BuildState    as BuildState (startState)
import Types.AssemblySchema
import Types.Error


build :: AssemblySchema -> Either CompilerError String
build schema = runBuildState buildAssembly schema BuildState.startState


buildAssembly :: AssemblySchema -> BuildState String
buildAssembly schema = buildASM schema


buildASM :: AssemblySchema -> BuildState String

buildASM (ProgramSchema topLeveltems) =
        concatMapM buildASM topLeveltems

buildASM (FunctionSchema name _) =
        pure $ prologue ++ epilogue
        where
                prologue = funcPrologue name
                epilogue = funcEpilogue

buildASM _                       = undefined
