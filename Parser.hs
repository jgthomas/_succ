
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
                    _ -> (ProgramNode funcTree, toks')


function :: [Token] -> (Tree, [Token])
function toks =
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Int] ->
                     case lookAhead (accept toks) of
                          (TokIdent id) ->
                                  let (stmentTree, toks') = statement (accept (accept toks))
                                      in (FunctionNode id stmentTree, toks')
                          _ -> error "No identifier supplied"
             _ -> statement toks


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
