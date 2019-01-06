
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
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Int] ->
                     let (funcTree, toks') = function (accept toks)
                         in (ProgramNode funcTree, toks')
             _ -> error "Invalid start of function"


function :: [Token] -> (Tree, [Token])
function toks =
        case lookAhead toks of
             (TokIdent id) ->
                     let (stmentTree, toks') = statement (accept toks)
                         in (FunctionNode id stmentTree, toks')
             _ -> error "No identifier supplied"


statement :: [Token] -> (Tree, [Token])
statement toks =
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Return] ->
                     let (exprsnTree, toks') = expression (accept toks)
                         in
                            if lookAhead toks' /= TokSemiColon
                            then error "Missing semicolon"
                            else (StatementNode kwd exprsnTree, accept toks')
             _ -> expression toks


expression :: [Token] -> (Tree, [Token])
expression toks =
        case lookAhead toks of
             (TokConstInt n)  ->  (ExpressionNode n, accept toks)
             _                ->  error $ "Parse error on token: " ++ show toks


funcParens :: [Token] -> Bool
funcParens (op:cl:toks) = op == TokOpenParen && cl == TokCloseParen
