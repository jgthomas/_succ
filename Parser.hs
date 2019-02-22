
module Parser (Tree(..), parse) where


import Lexer


data Tree = ProgramNode [Tree]
          | FunctionNode String [Tree] [Tree]
          | DeclarationNode String (Maybe Tree)
          | CompoundStmtNode [Tree]               -- statements
          | ReturnNode Tree
          | AssignmentNode String Tree Operator
          | ExprStmtNode Tree
          | IfNode Tree Tree (Maybe Tree)
          | WhileNode Tree Tree
          | DoWhileNode Tree Tree
          | ForLoopNode Tree Tree Tree Tree
          | BreakNode
          | ContinueNode
          | ConstantNode Int                      -- expressions
          | NullExprNode
          | VarNode String
          | UnaryNode Tree Operator
          | BinaryNode Tree Tree Operator
          | TernaryNode Tree Tree Tree
          | AssignNode String Tree
          deriving Show


parse :: [Token] -> Tree
parse toks = let (tree, toks') = parseProgram toks
                 in if null toks'
                       then tree
                       else error $ "Unparsed tokens: " ++ show toks


parseProgram :: [Token] -> (Tree, [Token])
parseProgram toks =
        let (funcList, toks') = parseAllFunctions [] toks
            in
        (ProgramNode funcList, toks')


parseAllFunctions :: [Tree] -> [Token] -> ([Tree], [Token])
parseAllFunctions funcList [] = (funcList, [])
parseAllFunctions funcList (typ:toks) =
        case typ of
             (TokKeyword typ)
                | validType typ ->
                        let (funcTree, toks') = parseFunction toks
                            in
                        parseAllFunctions (funcList ++ [funcTree]) toks'
                | otherwise -> error "Invalid type for function"
             _ -> error "Invalid function start"


parseFunction :: [Token] -> (Tree, [Token])
parseFunction (id:toks) =
        case id of
             (TokIdent funcName) ->
                     if lookAhead toks /= TokOpenParen
                        then error "Missing opening parenthesis"
                        else
                     let (funcParams, toks') = parseFunctionParams [] toks
                         in
                     if lookAhead toks' /= TokOpenBrace
                        then error "Missing opening brace"
                        else
                     let (funcBlockItems, toks'') = parseBlock [] (accept toks')
                         in
                     if lookAhead toks'' /= TokCloseBrace
                        then error "Missing closing brace"
                        else (FunctionNode funcName funcParams funcBlockItems, accept toks'')
             _ -> error "No identifier supplied"


parseFunctionParams :: [Tree] -> [Token] -> ([Tree], [Token])
parseFunctionParams paramList (first:second:toks)
        | first == TokCloseParen                       = (paramList, (second:toks))
        | first /= TokOpenParen && first /= TokComma   = error "Missing comma between parameters"
        | first == TokComma && second == TokCloseParen = error "Expected parameter type"
        | otherwise = case second of
                           TokCloseParen -> (paramList, toks)
                           (TokKeyword typ)
                              | validType typ ->
                                     let (paramTree, toks') = parseExpression toks
                                         in
                                     case paramTree of
                                          VarNode str -> parseFunctionParams
                                                         (paramList ++ [paramTree])
                                                         toks'
                                          _           -> error "Invalid function parameter"
                              | otherwise -> error "Invalid parameter type"
                           _ -> error "Invalid parameter"


parseBlock :: [Tree] -> [Token] -> ([Tree], [Token])
parseBlock stmts toks =
        case lookAhead toks of
             TokCloseBrace -> (stmts, toks)
             _ ->
                     let (nextStmt, toks') = parseBlockItem toks
                         in
                     parseBlock (stmts ++ [nextStmt]) toks'


parseBlockItem :: [Token] -> (Tree, [Token])
parseBlockItem toks =
        case lookAhead toks of
             (TokKeyword kwd) | kwd == Int     -> parseDeclaration toks
             _                                 -> parseStatement toks


parseStatement :: [Token] -> (Tree, [Token])
parseStatement (first:toks) =
        case first of
             TokKeyword Return   -> parseReturnStmt (first:toks)
             TokKeyword If       -> parseIfStatement (first:toks)
             TokKeyword While    -> parseWhileStatement (first:toks)
             TokKeyword Do       -> parseDoWhileStatement (first:toks)
             TokKeyword For      -> parseForLoop (first:toks)
             TokKeyword Break    -> parseBreak toks
             TokKeyword Continue -> parseContinue toks
             TokOpenBrace        -> parseCompoundStmt (first:toks)
             TokSemiColon        -> parseExpression (first:toks)
             TokIdent id         -> parseVariable (first:toks)
             _                   -> parseExprStatement (first:toks)


parseVariable :: [Token] -> (Tree, [Token])
parseVariable toks =
        let (exprTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokSemiColon
           then error "Missing semicolon"
           else (exprTree, accept toks')


parseBreak :: [Token] -> (Tree, [Token])
parseBreak (first:toks) =
        if first /= TokSemiColon
           then error "Missing semicolon"
           else (BreakNode, toks)


parseContinue :: [Token] -> (Tree, [Token])
parseContinue (first:toks) =
        if first /= TokSemiColon
           then error "Missing semicolon"
           else (ContinueNode, toks)


parseCompoundStmt :: [Token] -> (Tree, [Token])
parseCompoundStmt toks =
        let (blockItems, toks') = parseBlock [] (accept toks)
            in
        if lookAhead toks' /= TokCloseBrace
           then error "Block missing closing brace"
           else (CompoundStmtNode blockItems, accept toks')


parseForLoop :: [Token] -> (Tree, [Token])
parseForLoop (kwd:op:toks) =
        if op /= TokOpenParen
           then error "Missing opening parenthesis"
           else
        let (initTree, toks') = parseBlockItem toks
            in
        let (testTree, toks'') = parseStatement toks'
            in
        let (changeTree, toks''') = parseForLoopPostExp toks''
            in
        case lookAhead toks''' of
             TokSemiColon  -> error "Too many clauses"
             TokCloseParen ->
                     let (stmtTree, toks'''') = parseStatement $ accept toks'''
                         in
                     case testTree of
                          NullExprNode ->
                             (ForLoopNode initTree (ConstantNode 1) changeTree stmtTree, toks'''')
                          _            ->
                             (ForLoopNode initTree testTree changeTree stmtTree, toks'''')
             _ -> error "Missing closing parenthesis"


parseForLoopPostExp :: [Token] -> (Tree, [Token])
parseForLoopPostExp toks =
        case lookAhead toks of
             TokSemiColon  -> error "Too many clauses"
             TokCloseParen -> nullExpr toks
             _             -> parseExpression toks


parseDoWhileStatement :: [Token] -> (Tree, [Token])
parseDoWhileStatement (kwd:ob:toks) =
        if ob /= TokOpenBrace
           then error "Do block missing opening brace"
           else
        let (stmtTree, toks') = parseStatement (ob:toks)
            in
        case toks' of
             (next:second:toks'')
                | next /= TokKeyword While -> error "Do block missing while condition"
                | second /= TokOpenParen   -> error "Missing opening parenthesis"
                | otherwise                ->
                    let (testTree, toks''') = parseExpression toks''
                        in
                    case toks''' of
                         (next:second:toks'''')
                            | next /= TokCloseParen  -> error "Missing closing parenthesis"
                            | second /= TokSemiColon -> error "Missing semicolon"
                            | otherwise              -> (DoWhileNode stmtTree testTree, toks'''')


parseWhileStatement :: [Token] -> (Tree, [Token])
parseWhileStatement (kwd:op:toks) =
        if op /= TokOpenParen
           then error "Missing opening parentheses"
           else
        let (testTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokCloseParen
           then error "Missing closing parentheses"
           else
        let (stmtTree, toks'') = parseStatement (accept toks')
            in
        (WhileNode testTree stmtTree, toks'')


parseIfStatement :: [Token] -> (Tree, [Token])
parseIfStatement (kwd:op:toks) =
        if op /= TokOpenParen
           then error "Missing opening parentheses"
           else
        let (testTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokCloseParen
           then error "Missing closing parentheses"
           else
        let (stmtTree, toks'') = parseStatement (accept toks')
            (possElse, toks''') = parseOptionalElse toks''
            in
        (IfNode testTree stmtTree possElse, toks''')


parseOptionalElse :: [Token] -> (Maybe Tree, [Token])
parseOptionalElse all@(next:toks) =
        case next of
             TokKeyword Else ->
                     let (elseTree, toks') = parseStatement toks
                         in
                     (Just elseTree, toks')
             _ -> (Nothing, all)


parseReturnStmt :: [Token] -> (Tree, [Token])
parseReturnStmt (rtn:toks) =
        let (exprsnTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokSemiColon
           then error "Missing semicolon"
           else (ReturnNode exprsnTree, accept toks')


parseDeclaration :: [Token] -> (Tree, [Token])
parseDeclaration (ty:id:toks) =
        case id of
             (TokIdent varName) ->
                     let (exprTree, toks') = parseOptionalAssign (id:toks)
                         in
                     if lookAhead toks' /= TokSemiColon
                        then error "Missing semicolon"
                        else (DeclarationNode varName exprTree, accept toks')


parseOptionalAssign :: [Token] -> (Maybe Tree, [Token])
parseOptionalAssign (id:equ:toks) =
        case equ of
             TokAssign ->
                     let (exprTree, toks') = parseExpression (id:equ:toks)
                         in
                     (Just exprTree, toks')
             _ -> (Nothing, (equ:toks))


parseExprStatement :: [Token] -> (Tree, [Token])
parseExprStatement toks =
        let (exprTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokSemiColon
           then error "Missing semicolon"
           else (ExprStmtNode exprTree, accept toks')


parseExpression :: [Token] -> (Tree, [Token])
parseExpression toks =
        let (expressTree, toks') = parseTernaryExpr toks
            in
        case lookAhead toks' of
             TokAssign ->
                     case expressTree of
                          VarNode str ->
                                  let (exTree, toks'') = parseExpression (accept toks')
                                      in
                                  (AssignmentNode str exTree Assign, toks'')
                          _ -> error "Can only assign to variables"
             _ -> (expressTree, toks')


parseTernaryExpr :: [Token] -> (Tree, [Token])
parseTernaryExpr toks =
        let (condTree, toks') = parseLogicalOrExp toks
            in
        case lookAhead toks' of
             TokQuestMark ->
                     let (exprTree, toks'') = parseExpression (accept toks')
                         in
                     if lookAhead toks'' /= TokColon
                        then error "Missing colon on ternary expression"
                        else
                     let (finalExprTree, toks''') = parseTernaryExpr (accept toks'')
                         in
                     (TernaryNode condTree exprTree finalExprTree, toks''')
             _ -> (condTree, toks')


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
        parseBinaryExp facTree toks' parseFactor [Multiply,Divide,Modulo]


parseFactor :: [Token] -> (Tree, [Token])
parseFactor all@(next:toks) =
        case next of
             (TokConstInt n) -> (ConstantNode n, toks)
             (TokIdent str)  -> (VarNode str, toks)
             TokSemiColon    -> (NullExprNode, toks)
             (TokOp op) | elem op [Minus, BitwiseCompl, LogicNegation] ->
                     let (facTree, toks') = parseFactor toks
                         in
                     (UnaryNode facTree op, toks')
             TokOpenParen ->
                     let (exprTree, toks') = parseExpression toks
                         in
                     if lookAhead toks' /= TokCloseParen
                        then error "Missing right parentheses"
                        else (exprTree, accept toks')
             _ ->  error $ "Parse error on token: " ++ show all


parseBinaryExp :: Tree
               -> [Token]
               -> ([Token] -> (Tree, [Token]))
               -> [Operator]
               -> (Tree, [Token])
parseBinaryExp tree toks nextVal ops =
        case lookAhead toks of
             (TokOp op) | elem op ops ->
                     let (nexTree, toks') = nextVal (accept toks)
                         in
                     parseBinaryExp (BinaryNode tree nexTree op) toks' nextVal ops
             _ -> (tree, toks)


nullExpr :: [Token] -> (Tree, [Token])
nullExpr toks = (NullExprNode, toks)


validType :: Keyword -> Bool
validType kwd = elem kwd [Int]
