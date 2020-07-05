
module CheckerTest.TestUtility (extractError, extractTree) where


import Checker.Checker (check)
import Types.AST
import Types.Error


extractTree :: Tree -> Tree
extractTree tree = getTree . check $ tree


extractError :: Tree -> CompilerError
extractError tree = getError . check $ tree


getTree :: Either CompilerError Tree -> Tree
getTree (Right tree) = tree
getTree (Left err)   = error $ show err


getError :: Either CompilerError Tree -> CompilerError
getError (Right tree) = error $ show tree
getError (Left err)   = err
