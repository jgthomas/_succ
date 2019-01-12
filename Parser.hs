
module Parser (Tree(..), parse) where


import Lexer


data Tree = ProgramNode [Tree]
          | FunctionNode String [Tree]
          | ReturnNode Tree                      -- statements
          | DeclStmtNode String (Maybe Tree)
          | ExprStmtNode Tree
          | ConstantNode Int                     -- expressions
          | UnaryNode Tree Operator
          | BinaryNode Tree Tree Operator
          | AssignNode String Tree
          deriving Show


parse :: [Token] -> Tree
parse toks = let (tree, toks') = parseProgram toks
                 in if null toks'
                       then tree
                       else error $ "Unparsed tokens: " ++ show toks


parseProgram :: [Token] -> (Tree, [Token])
parseProgram toks =
        if isValidType toks
           then let (funcTree, toks') = parseFunction (accept toks)
                    in
           (ProgramNode [funcTree], toks')
           else error "Invalid start of function"


parseFunction :: [Token] -> (Tree, [Token])
parseFunction toks =
        case lookAhead toks of
             (TokIdent id) | isFuncStart (accept toks) ->
                     let (stmentTree, toks') = parseStatement (drop 4 toks)
                         (stmentList, toks'') = parseAllStatements [stmentTree] toks'
                         in
                     if lookAhead toks'' /= TokCloseBrace
                        then error "Missing closing brace"
                        else (FunctionNode id stmentList, accept toks'')
             _ -> error "No identifier supplied"


parseAllStatements :: [Tree] -> [Token] -> ([Tree], [Token])
parseAllStatements stmts toks =
        case lookAhead toks of
             TokCloseBrace -> (stmts, toks)
             _ ->
                     let (nextStmt, toks') = parseStatement toks
                         in
                     parseAllStatements (stmts ++ [nextStmt]) toks'


parseStatement :: [Token] -> (Tree, [Token])
parseStatement toks =
        case lookAhead toks of
             (TokKeyword kwd) | kwd == Return ->
                              let (stmtTree, toks') = parseReturnStmt toks
                                  in
                              if lookAhead toks' /= TokSemiColon
                                 then error "Missing semicolon"
                                 else (stmtTree, accept toks')
                              | elem kwd [Int] ->
                              let (stmtTree, toks') = parseDeclStmt toks
                                  in
                              if lookAhead toks' /= TokSemiColon
                                 then error "Missing semicolon"
                                 else (stmtTree, accept toks')


parseReturnStmt :: [Token] -> (Tree, [Token])
parseReturnStmt toks =
        let (exprsnTree, toks') = parseExpression (accept toks)
            in
        (ReturnNode exprsnTree, toks')


parseDeclStmt :: [Token] -> (Tree, [Token])
parseDeclStmt (ty:id:toks) =
        case id of
             (TokIdent varName) ->
                     let (exprTree, toks') = parseMaybeExpr toks
                         in
                     (DeclStmtNode varName exprTree, toks')


parseMaybeExpr :: [Token] -> (Maybe Tree, [Token])
parseMaybeExpr (equ:toks) =
        case equ of
             TokAssign ->
                     let (exprTree, toks') = parseExprStmt (equ:toks)
                         in
                     (Just exprTree, toks')
             _ -> (Nothing, (equ:toks))


parseExprStmt :: [Token] -> (Tree, [Token])
parseExprStmt (equ:toks) =
        let (exprTree, toks') = parseExpression toks
            in
        (ExprStmtNode exprTree, toks')


parseExpression :: [Token] -> (Tree, [Token])
parseExpression toks = parseLogicalOrExp toks


parseLogicalOrExp :: [Token] -> (Tree, [Token])
parseLogicalOrExp toks =
        let (orTree, toks') = parseLogicalAndExp toks
            in
        case lookAhead toks' of
             (TokOp op) | op == LogicalOR ->
                        parseBinaryExp orTree toks' parseLogicalAndExp
             _ -> (orTree, toks')


parseLogicalAndExp :: [Token] -> (Tree, [Token])
parseLogicalAndExp toks =
        let (andTree, toks') = parseEqualityExp toks
            in
        case lookAhead toks' of
             (TokOp op) | op == LogicalAND ->
                        parseBinaryExp andTree toks' parseEqualityExp
             _ -> (andTree, toks')


parseEqualityExp :: [Token] -> (Tree, [Token])
parseEqualityExp toks =
        let (equTree, toks') = parseRelationalExp toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [Equal,NotEqual] ->
                     parseBinaryExp equTree toks' parseRelationalExp
             _ -> (equTree, toks')


parseRelationalExp :: [Token] -> (Tree, [Token])
parseRelationalExp toks =
        let (relaTree, toks') = parseAdditiveExp toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [GreaterThan,LessThan,GreaterThanOrEqual,LessThanOrEqual] ->
                     parseBinaryExp relaTree toks' parseAdditiveExp
             _ -> (relaTree, toks')


parseAdditiveExp :: [Token] -> (Tree, [Token])
parseAdditiveExp toks =
        let (termTree, toks') = parseTerm toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [Plus, Minus] ->
                     parseBinaryExp termTree toks' parseTerm
             _ -> (termTree, toks')


parseTerm :: [Token] -> (Tree, [Token])
parseTerm toks =
        let (facTree, toks') = parseFactor toks
            in
        case lookAhead toks' of
             (TokOp op) | elem op [Multiply, Divide] ->
                     parseBinaryExp facTree toks' parseFactor
             _ -> (facTree, toks')


parseFactor :: [Token] -> (Tree, [Token])
parseFactor toks =
        case lookAhead toks of
             (TokConstInt n) -> (ConstantNode n, (accept toks))
             (TokOp op) | elem op [Minus, BitwiseCompl, LogicNegation] ->
                     let (facTree, toks') = parseFactor (accept toks)
                         in
                     (UnaryNode facTree op, toks')
             TokOpenParen ->
                     let (exprTree, toks') = parseExpression (accept toks)
                         in
                     if lookAhead toks' /= TokCloseParen
                        then error "Missing right parentheses"
                        else (exprTree, accept toks')
             _ ->  error $ "Parse error on token: " ++ show toks


parseBinaryExp :: Tree -> [Token] -> ([Token] -> (Tree, [Token])) -> (Tree, [Token])
parseBinaryExp tree toks nextVal =
        case lookAhead toks of
             (TokOp op) ->
                     let (nexTree, toks') = nextVal (accept toks)
                         in
                     parseBinaryExp (BinaryNode tree nexTree op) toks' nextVal
             _ -> (tree, toks)


isFuncStart :: [Token] -> Bool
isFuncStart (op:cp:ob:toks)
    | op /= TokOpenParen  = error "Missing opening parenthesis"
    | cp /= TokCloseParen = error "Missing closing parenthesis"
    | ob /= TokOpenBrace  = error "Missing opening brace"
    | otherwise           = True


isValidType :: [Token] -> Bool
isValidType toks =
        case lookAhead toks of
             (TokKeyword kwd) | elem kwd [Int] -> True
             _                                 -> False


validType :: Token -> Bool
validType (TokKeyword kwd) = elem kwd [Int]
