
module Error.MessageScopeError (scopeErrorMsg) where


import Error.PrintErrorTokens (PrintRange (..), buildLineMsg)
import Types.AST              (NodeDat (..), Tree (..))
import Types.Error            (ScopeError (..))


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
