
module Parser (Tree(..), parse) where


import Lexer


data Tree = ProgramNode [Tree]
          | FunctionNode String [Tree]
          | ReturnNode Tree                      -- statements
          | DeclarationNode Tree (Maybe Tree)
          | ExpressionStatementNode Tree Tree Operator
          | ConstantNode Int                     -- expressions
          | VarNode String
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
             (TokKeyword kwd) | kwd == Return -> parseReturnStmt toks
                              | kwd == Int    -> parseDeclStmt toks
             (TokIdent id) -> let (exprTree, toks') = parseExpression toks
                                  in
                              if lookAhead toks' /= TokSemiColon
                                 then error "Missing semicolon"
                                 else (exprTree, accept toks')


parseReturnStmt :: [Token] -> (Tree, [Token])
parseReturnStmt (rtn:toks) =
        let (exprsnTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokSemiColon
           then error "Missing semicolon"
           else (ReturnNode exprsnTree, accept toks')


parseDeclStmt :: [Token] -> (Tree, [Token])
parseDeclStmt (ty:id:toks) =
        case id of
             (TokIdent varName) ->
                     let (exprTree, toks') = parseOptionalAssign (id:toks)
                         in
                     if lookAhead toks' /= TokSemiColon
                        then error "Missing semicolon"
                        else (DeclarationNode (VarNode varName) exprTree, accept toks')


parseOptionalAssign :: [Token] -> (Maybe Tree, [Token])
parseOptionalAssign (id:equ:toks) =
        case equ of
             TokAssign ->
                     let (exprTree, toks') = parseExpression (id:equ:toks)
                         in
                     (Just exprTree, toks')
             _ -> (Nothing, (equ:toks))


parseExpression :: [Token] -> (Tree, [Token])
parseExpression toks =
        let (expressTree, toks') = parseLogicalOrExp toks
            in
        case lookAhead toks' of
             TokAssign ->
                     case expressTree of
                          VarNode str ->
                                  let (exTree, toks'') = parseExpression (accept toks')
                                      in
                                  (ExpressionStatementNode expressTree exTree Assign, toks'')
                          _ -> error "Can only assign to variables"
             _ -> (expressTree, toks')


parseLogicalOrExp :: [Token] -> (Tree, [Token])
parseLogicalOrExp toks =
        let (orTree, toks') = parseLogicalAndExp toks
            in
        parseBinaryExp orTree toks' parseLogicalAndExp [LogicalOR]


parseLogicalAndExp :: [Token] -> (Tree, [Token])
parseLogicalAndExp toks =
        let (andTree, toks') = parseEqualityExp toks
            in
        parseBinaryExp andTree toks' parseEqualityExp [LogicalAND]


parseEqualityExp :: [Token] -> (Tree, [Token])
parseEqualityExp toks =
        let (equTree, toks') = parseRelationalExp toks
            in
        parseBinaryExp equTree toks' parseRelationalExp [Equal,NotEqual]


parseRelationalExp :: [Token] -> (Tree, [Token])
parseRelationalExp toks =
        let (relaTree, toks') = parseAdditiveExp toks
            in
        parseBinaryExp relaTree toks' parseAdditiveExp
             [GreaterThan,LessThan,GreaterThanOrEqual,LessThanOrEqual]


parseAdditiveExp :: [Token] -> (Tree, [Token])
parseAdditiveExp toks =
        let (termTree, toks') = parseTerm toks
            in
        parseBinaryExp termTree toks' parseTerm [Plus,Minus]


parseTerm :: [Token] -> (Tree, [Token])
parseTerm toks =
        let (facTree, toks') = parseFactor toks
            in
        parseBinaryExp facTree toks' parseFactor [Multiply,Divide]


parseFactor :: [Token] -> (Tree, [Token])
parseFactor toks =
        case lookAhead toks of
             (TokConstInt n) -> (ConstantNode n, (accept toks))
             (TokIdent str)  -> (VarNode str, accept toks)
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


parseBinaryExp :: Tree -> [Token] -> ([Token] -> (Tree, [Token])) -> [Operator] -> (Tree, [Token])
parseBinaryExp tree toks nextVal ops =
        case lookAhead toks of
             (TokOp op) | elem op ops ->
                     let (nexTree, toks') = nextVal (accept toks)
                         in
                     parseBinaryExp (BinaryNode tree nexTree op) toks' nextVal ops
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


getName :: Token -> String
getName (TokIdent n) = n
