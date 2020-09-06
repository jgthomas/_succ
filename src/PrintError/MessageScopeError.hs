{-|
Module       : MessageScopeError
Description  : Format scope error messages

Formats error messages of the scope error type
-}
module PrintError.MessageScopeError (scopeErrorMsg) where


import PrintError.PrintErrorTokens (PrintRange (..), buildLineMsg)
import Types.AST                   (NodeDat (..), Tree (..))
import Types.Error                 (ScopeError (..))


-- | Generate scope error message
scopeErrorMsg :: ScopeError -> (String, PrintRange)

scopeErrorMsg (DoubleDefinedNode (FunctionNode _ name _ _ dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Identifier '" ++ name ++ "' already defined"

scopeErrorMsg (UnexpectedNode (BreakNode dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'break' outside loop context"

scopeErrorMsg (UnexpectedNode (ContinueNode dat)) = (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Unexpected 'continue' outside loop context"

scopeErrorMsg (UndeclaredNode (FuncCallNode name _ dat)) =
              (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Calling undeclared function '" ++ name ++ "'"

scopeErrorMsg err = (show err, All)
