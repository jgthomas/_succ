
module Builder.Builder (build) where


import Builder.BuildFunction (funcEpilogue, funcPrologue)
import Types.AssemblySchema


build :: AssemblySchema -> String
build schema = buildSchema schema


buildSchema :: AssemblySchema -> String

buildSchema (ProgramSchema topLeveltems) =
        concatMap buildSchema topLeveltems

buildSchema (FunctionSchema name _) =
        prologue ++ epilogue
        where
                prologue = funcPrologue name
                epilogue = funcEpilogue

buildSchema _                       = undefined
