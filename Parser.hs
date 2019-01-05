
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
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Return] ->
                     let (exprsnTree, toks') = expression (accept toks)
                         in (StatementNode kwd exprsnTree, toks')
             _ -> expression toks


expression :: [Token] -> (Tree, [Token])
expression toks =
        let (tokTree, toks') = token toks
            in case lookAhead toks' of
                    _ -> (tokTree, toks')


token :: [Token] -> (Tree, [Token])
token toks =
        case lookAhead toks of
             (TokConstInt n)  ->  (ExpressionNode n, accept toks)
             _                ->  error $ "Parse error on token: " ++ show toks
