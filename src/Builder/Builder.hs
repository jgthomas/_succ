
module Builder.Builder (build) where


import Builder.BuildFunction (funcEpilogue, funcPrologue)
import Types.AssemblySchema


build :: AssemblySchema -> String
build schema = buildSchema schema


buildSchema :: AssemblySchema -> String
buildSchema schema@FunctionSchema{} = buildFunctionSchema schema
buildSchema _                       = undefined


buildFunctionSchema :: AssemblySchema -> String
buildFunctionSchema (FunctionSchema name _) =
        funcPrologue name ++ funcEpilogue
buildFunctionSchema _ = undefined
