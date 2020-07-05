
module GeneratorTest.TestUtility (extractAssembly) where


import Generator.Generator (generate)
import State.SymTab        (SymTab)
import Types.AST
import Types.Error


extractAssembly :: Tree -> String
extractAssembly tree = getASM . generate $ tree


getASM :: Either CompilerError (String, SymTab) -> String
getASM (Right (asm, _)) = asm
getASM (Left err)       = error $ show err
