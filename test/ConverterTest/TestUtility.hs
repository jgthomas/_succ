
module ConverterTest.TestUtility (extractSchema) where


import Converter.Converter  (convert)
import State.State          (SymTab)
import Types.AssemblySchema
import Types.AST
import Types.Error


extractSchema :: Tree -> AssemblySchema
extractSchema tree = getSchema . convert $ tree


getSchema :: Either CompilerError (AssemblySchema, SymTab) -> AssemblySchema
getSchema (Right (asm, _)) = asm
getSchema (Left err)       = error $ show err

