module ConverterTest.TestUtility
  ( extractSchema,
    extractError,
  )
where

import Converter.Converter (convert)
import State.State (SymTab)
import Types.AST
import Types.AssemblySchema
import Types.Error

extractSchema :: Tree -> AssemblySchema
extractSchema tree = getSchema . convert $ tree

extractError :: Tree -> CompilerError
extractError tree = getError . convert $ tree

getSchema :: Either CompilerError (AssemblySchema, SymTab) -> AssemblySchema
getSchema (Right (asm, _)) = asm
getSchema (Left err) = error $ show err

getError :: Either CompilerError (AssemblySchema, SymTab) -> CompilerError
getError (Right (asm, _)) = error $ show asm
getError (Left err) = err
