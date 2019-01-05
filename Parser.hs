
module Parser (Tree(..), parse) where


import Lexer


data Tree = ProgramNode Tree
          | FunctionNode String Tree
          | StatementNode Keyword Tree
          | ExpressionNode Int
          deriving Show


parse :: [Token] -> Tree
parse toks = let (tree, toks') = program toks
                 in if null toks'
                       then tree
                       else error $ "Unparsed tokens: " ++ show toks


program :: [Token] -> (Tree, [Token])
program toks =
        let (funcTree, toks') = function toks
            in case lookAhead toks' of
                    _ -> (funcTree, toks')


function :: [Token] -> (Tree, [Token])
function toks =
        let (stmentTree, toks') = statement toks
            in case lookAhead toks' of
                    _ -> (stmentTree, toks')


statement :: [Token] -> (Tree, [Token])
statement toks =
        let (exprsnTree, toks') = expression toks
            in case lookAhead toks' of
                    _ -> (exprsnTree, toks')


expression :: [Token] -> (Tree, [Token])
expression toks =
        case lookAhead toks of
             (TokConstInt n)  ->  (ExpressionNode n, accept toks)
             _                ->  error $ "Parse error on token: " ++ show toks


--token :: [Token] -> (Tree, [Token])
--token toks =
--        case lookAhead toks of
--             (TokConstInt n)  ->  (Expression n, accept toks)
--             _                ->  error $ "Parse error on token: " ++ show toks
