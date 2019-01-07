
module Parser (Tree(..), parse) where


import Lexer


data Tree = ProgramNode Tree
          | FunctionNode String Tree
          | ReturnNode Tree
          | ConstantNode Int
          | UnaryNode Tree Operator
          | BinaryNode Tree Tree Operator
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
             (TokIdent id) | isFuncStart (accept toks) ->
                     let (stmentTree, toks') = statement (drop 4 toks)
                         in
                     if lookAhead toks' /= TokCloseBrace
                        then error "Missing closing brace"
                        else (FunctionNode id stmentTree, accept toks')
             _ -> error "No identifier supplied"


statement :: [Token] -> (Tree, [Token])
statement toks =
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Return] ->
                     let (exprsnTree, toks') = expression (accept toks)
                         in
                            if lookAhead toks' /= TokSemiColon
                            then error "Missing semicolon"
                            else (ReturnNode exprsnTree, accept toks')
             _ -> expression toks


expression :: [Token] -> (Tree, [Token])
expression toks =
        let (firstTermTree, toks') = term toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [Plus, Minus] ->
                     parseBin firstTermTree toks'
             _ -> (firstTermTree, toks')


parseBin :: Tree -> [Token] -> (Tree, [Token])
parseBin tree toks =
        case lookAhead toks of
             (TokOp op) | elem op [Plus, Minus] ->
                     let (termTree, toks') = factor (accept toks)
                         in
                     parseBin (BinaryNode tree termTree op) toks'
             _ -> (tree, toks)


term :: [Token] -> (Tree, [Token])
term toks =
        let (facTree, toks') = factor toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [Multiply, Divide] ->
                     let (termTree, toks'') = term (accept toks')
                         in
                     (BinaryNode facTree termTree op, toks'')
             _ -> (facTree, toks')


factor :: [Token] -> (Tree, [Token])
factor toks =
        case lookAhead toks of
             (TokConstInt n) -> (ConstantNode n, (accept toks))
             (TokOp op) | elem op [Minus, BitwiseCompl, LogicNegation] ->
                     let (facTree, toks') = factor (accept toks)
                         in
                     (UnaryNode facTree op, toks')
             TokOpenParen ->
                     let (exprTree, toks') = expression (accept toks)
                         in
                     if lookAhead toks' /= TokCloseParen
                        then error "Missing right parentheses"
                        else (exprTree, accept toks')
             _ ->  error $ "Parse error on token: " ++ show toks


isFuncStart :: [Token] -> Bool
isFuncStart (op:cp:ob:toks)
    | op /= TokOpenParen  = error "Missing opening parenthesis"
    | cp /= TokCloseParen = error "Missing closing parenthesis"
    | ob /= TokOpenBrace  = error "Missing opening brace"
    | otherwise           = True
