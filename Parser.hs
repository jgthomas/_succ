
module Parser (parse) where


import AST    (Tree(..))
import Types  (Type(..))
import Tokens (Operator(..), Keyword(..), Token(..))


parse :: [Token] -> Tree
parse toks = let (tree, toks') = parseProgram toks
                 in if null toks'
                       then tree
                       else error $ "Unparsed tokens: " ++ show toks


parseProgram :: [Token] -> (Tree, [Token])
parseProgram toks =
        let (funcList, toks') = parseTopLevelItems [] toks
            in
        (ProgramNode funcList, toks')


parseTopLevelItems :: [Tree] -> [Token] -> ([Tree], [Token])
parseTopLevelItems itemList [] = (itemList, [])
parseTopLevelItems itemList allToks@(a:toks) =
        case a of
             (TokKeyword typ)
                | validType typ ->
                        let (item, toks') = parseTopLevelItem allToks
                            in
                        parseTopLevelItems (itemList ++ [item]) toks'
                | otherwise -> error $ errorMessage TypeError ++ show typ
             _ -> error $ errorMessage TypeError ++ show a


parseTopLevelItem :: [Token] -> (Tree, [Token])
parseTopLevelItem allToks@(a:b:c:toks) =
        case c of
             TokOpenParen -> parseFunction . accept $ allToks
             _            -> parseDeclaration allToks


parseFunction :: [Token] -> (Tree, [Token])
parseFunction (id:toks) =
        case id of
             (TokIdent funcName) ->
                     if lookAhead toks /= TokOpenParen
                        then error $ errorMessage OpenParen
                        else
                     let (funcParams, toks') = parseFunctionParams [] toks
                         in
                     case lookAhead toks' of
                          TokSemiColon -> (FunctionProtoNode funcName funcParams, accept toks')
                          TokOpenBrace ->
                                  let (funcBlockItems, toks'') = parseBlock [] . accept $ toks'
                                      in
                                  if lookAhead toks'' /= TokCloseBrace
                                     then error $ errorMessage CloseBrace
                                     else (FunctionNode funcName funcParams funcBlockItems, accept toks'')
                          _ -> error "Invalid function declaration"
             _ -> error "No identifier supplied"


parseFunctionParams :: [Tree] -> [Token] -> ([Tree], [Token])
parseFunctionParams paramList allToks@(first:second:toks)
        | first == TokCloseParen                       = (paramList, second:toks)
        | first /= TokOpenParen && first /= TokComma   = error "Missing comma between parameters"
        | first == TokComma && second == TokCloseParen = error "Expected parameter type"
        | otherwise = case second of
                           TokCloseParen -> (paramList, toks)
                           (TokKeyword typ)
                              | validType typ ->
                                     let (paramTree, toks') = parseParam . accept $ allToks
                                         in
                                     parseFunctionParams (paramList ++ [paramTree]) toks'
                              | otherwise -> error "Invalid parameter type"
                           _ -> error "Invalid parameter keyword"


parseParam :: [Token] -> (Tree, [Token])
parseParam allToks@(typ:ast:toks) =
        let (paramTree, toks') = parseParamValue . accept $ allToks
            in
        case paramTree of
             VarNode str -> (ParamNode (setType typ ast) paramTree, toks')
             _           -> error "Invalid function parameter"


parseParamValue :: [Token] -> (Tree, [Token])
parseParamValue allToks@(ast:toks) =
        case ast of
             (TokOp Multiply) -> parseExpression toks
             (TokIdent a)     -> parseExpression allToks


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
             (TokKeyword kwd) | validType kwd -> parseDeclaration toks
             _                                -> parseStatement toks


parseStatement :: [Token] -> (Tree, [Token])
parseStatement allToks@(first:toks) =
        case first of
             TokKeyword Return   -> parseReturnStmt toks
             TokKeyword If       -> parseIfStatement toks
             TokKeyword While    -> parseWhileStatement toks
             TokKeyword Do       -> parseDoWhileStatement toks
             TokKeyword For      -> parseForLoop toks
             TokKeyword Break    -> parseBreak toks
             TokKeyword Continue -> parseContinue toks
             TokOpenBrace        -> parseCompoundStmt toks
             TokOp Multiply      -> parseExpression allToks
             TokIdent id         ->
                     case lookAhead toks of
                          (TokOp op)
                               | isAssignment op -> parseExprStatement allToks
                               | otherwise       -> parseExpression allToks
                          TokOpenParen -> parseExprStatement allToks
                          _            -> parseExpression allToks
             _ -> parseExprStatement allToks


{-
- Parses expressions where a semi-colon is required afterwards
-
- null expression:         ;
- expression statements:   2 + 2;
- elements of loops:       (i = 0; i < 10; i++)
- assignments:             a = 10;
- function calls:          dog(8);
-
-}
parseExprStatement :: [Token] -> (Tree, [Token])
parseExprStatement allToks@(first:toks) =
        case first of
             TokSemiColon -> parseNullStatement toks
             _            ->
                     let (exprTree, toks') = parseExpression allToks
                         in
                     if lookAhead toks' /= TokSemiColon
                        then error $ errorMessage SemiColon
                        else (ExprStmtNode exprTree, accept toks')


parseBreak :: [Token] -> (Tree, [Token])
parseBreak (first:toks) =
        if first /= TokSemiColon
           then error $ errorMessage SemiColon
           else (BreakNode, toks)


parseContinue :: [Token] -> (Tree, [Token])
parseContinue (first:toks) =
        if first /= TokSemiColon
           then error $ errorMessage SemiColon
           else (ContinueNode, toks)


parseCompoundStmt :: [Token] -> (Tree, [Token])
parseCompoundStmt toks =
        let (blockItems, toks') = parseBlock [] toks
            in
        if lookAhead toks' /= TokCloseBrace
           then error $ errorMessage CloseBrace
           else (CompoundStmtNode blockItems, accept toks')


parseForLoop :: [Token] -> (Tree, [Token])
parseForLoop (first:toks) =
        if first /= TokOpenParen
           then error $ errorMessage CloseParen
           else
        let (initTree, toks') = parseBlockItem toks
            in
        let (testTree, toks'') = parseExprStatement toks'
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
             _ -> error $ errorMessage CloseParen


parseForLoopPostExp :: [Token] -> (Tree, [Token])
parseForLoopPostExp toks =
        case lookAhead toks of
             TokSemiColon  -> error "Too many clauses"
             TokCloseParen -> nullExpr toks
             _             -> parseExpression toks


parseDoWhileStatement :: [Token] -> (Tree, [Token])
parseDoWhileStatement allToks@(first:toks) =
        if first /= TokOpenBrace
           then error "Do block missing opening brace"
           else
        let (stmtTree, toks') = parseStatement allToks
            in
        case toks' of
             (next:second:toks'')
                | next /= TokKeyword While -> error "Do block missing while condition"
                | second /= TokOpenParen   -> error $ errorMessage OpenParen
                | otherwise                ->
                    let (testTree, toks''') = parseExpression toks''
                        in
                    case toks''' of
                         (next:second:toks'''')
                            | next /= TokCloseParen  -> error $ errorMessage CloseParen
                            | second /= TokSemiColon -> error $ errorMessage SemiColon
                            | otherwise              -> (DoWhileNode stmtTree testTree, toks'''')


parseWhileStatement :: [Token] -> (Tree, [Token])
parseWhileStatement (first:toks) =
        if first /= TokOpenParen
           then error $ errorMessage OpenParen
           else
        let (testTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokCloseParen
           then error $ errorMessage CloseParen
           else
        let (stmtTree, toks'') = parseStatement . accept $ toks'
            in
        (WhileNode testTree stmtTree, toks'')


parseIfStatement :: [Token] -> (Tree, [Token])
parseIfStatement (first:toks) =
        if first /= TokOpenParen
           then error $ errorMessage OpenParen
           else
        let (testTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokCloseParen
           then error $ errorMessage CloseParen
           else
        let (stmtTree, toks'') = parseStatement . accept $ toks'
            (possElse, toks''') = parseOptionalElse toks''
            in
        (IfNode testTree stmtTree possElse, toks''')


parseOptionalElse :: [Token] -> (Maybe Tree, [Token])
parseOptionalElse allToks@(next:toks) =
        case next of
             TokKeyword Else ->
                     let (elseTree, toks') = parseStatement toks
                         in
                     (Just elseTree, toks')
             _ -> (Nothing, allToks)


parseReturnStmt :: [Token] -> (Tree, [Token])
parseReturnStmt toks =
        let (exprsnTree, toks') = parseExpression toks
            in
        if lookAhead toks' /= TokSemiColon
           then error $ errorMessage SemiColon
           else (ReturnNode exprsnTree, accept toks')


parseNullStatement :: [Token] -> (Tree, [Token])
parseNullStatement toks = (NullExprNode, toks)


parseDeclaration :: [Token] -> (Tree, [Token])
parseDeclaration allToks@(typ:id:toks) =
        case id of
             (TokOp Multiply)   -> parsePointerDec allToks
             (TokIdent varName) ->
                     let (exprTree, toks') = parseOptAssign . accept $ allToks
                         in
                     (DeclarationNode varName (setType typ id) exprTree, toks')
             _ -> error $ "invalid identifier: " ++ show id


parsePointerDec :: [Token] -> (Tree, [Token])
parsePointerDec allToks@(typ:ast:id:toks) =
        case id of
             (TokIdent varName) ->
                     let (exprTree, toks') = parseOptAssign (id:toks)
                         in
                     (PointerNode varName (setType typ ast) exprTree, toks')
             _ -> error $ "invalid identifier: " ++ show id


parseOptAssign :: [Token] -> (Maybe Tree, [Token])
parseOptAssign toks =
        let (exprTree, toks') = parseOptionalAssign toks
            in
        if lookAhead toks' /= TokSemiColon
           then error $ errorMessage SemiColon
           else (exprTree, accept toks')


parseOptionalAssign :: [Token] -> (Maybe Tree, [Token])
parseOptionalAssign allToks@(id:equ:toks) =
        case equ of
             (TokOp op)
                | isAssignment op ->
                     let (exprTree, toks') = parseExpression allToks
                         in
                     (Just exprTree, toks')
             _ -> (Nothing, equ:toks)


parseExpression :: [Token] -> (Tree, [Token])
parseExpression toks =
        let (expressTree, toks') = parseTernaryExpr toks
            in
        case lookAhead toks' of
             (TokOp op)
                | isAssignment op ->
                        case expressTree of
                             (VarNode id) ->
                                     let (exTree, toks'') = parseExpression . accept $ toks'
                                         in
                                     (AssignmentNode id exTree op, toks'')
                             (DereferenceNode id) ->
                                     let (exTree, toks'') = parseExpression . accept $ toks'
                                         in
                                     (AssignDereferenceNode id exTree op, toks'')
                             _ -> error $ "cannot assign to : " ++ show expressTree
                | otherwise -> error "invalid assignment operator"
             _ -> (expressTree, toks')


parseTernaryExpr :: [Token] -> (Tree, [Token])
parseTernaryExpr toks =
        let (condTree, toks') = parseLogicalOrExp toks
            in
        case lookAhead toks' of
             TokQuestMark ->
                     let (exprTree, toks'') = parseExpression . accept $ toks'
                         in
                     if lookAhead toks'' /= TokColon
                        then error "Missing colon on ternary expression"
                        else
                     let (finalExprTree, toks''') = parseTernaryExpr . accept $ toks''
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
parseFactor allToks@(next:toks) =
        case next of
             TokSemiColon    -> (NullExprNode, toks)
             (TokConstInt n) -> (ConstantNode n, toks)
             (TokIdent str)  ->
                     if lookAhead toks == TokOpenParen
                        then parseFunctionCall allToks
                        else (VarNode str, toks)
             (TokOp op)
                | op == Ampersand -> parseAddressOf toks
                | op == Multiply  -> parseDereference toks
                | op `elem` [Minus, BitwiseCompl, LogicNegation] ->
                        let (facTree, toks') = parseFactor toks
                            in
                        (UnaryNode facTree op, toks')
             TokOpenParen ->
                     let (exprTree, toks') = parseExpression toks
                         in
                     if lookAhead toks' /= TokCloseParen
                        then error $ errorMessage CloseParen
                        else (exprTree, accept toks')
             _ ->  error $ errorMessage ParseError ++ show allToks


parseBinaryExp :: Tree
               -> [Token]
               -> ([Token] -> (Tree, [Token]))
               -> [Operator]
               -> (Tree, [Token])
parseBinaryExp tree toks nextVal ops =
        case lookAhead toks of
             (TokOp op) | op `elem` ops ->
                     let (nexTree, toks') = nextVal . accept $ toks
                         in
                     parseBinaryExp (BinaryNode tree nexTree op) toks' nextVal ops
             _ -> (tree, toks)


parseAddressOf :: [Token] -> (Tree, [Token])
parseAddressOf (id:toks) =
        case id of
             (TokIdent n) -> (AddressOfNode n, toks)
             _            -> error $ "invalid identifier: " ++ show id


parseDereference :: [Token] -> (Tree, [Token])
parseDereference (id:toks) =
        case id of
             (TokIdent n) -> (DereferenceNode n, toks)
             _            -> error $ "invalid identifier: " ++ show id


parseFunctionCall :: [Token] -> (Tree, [Token])
parseFunctionCall allToks@(id:paren:toks) =
        if paren /= TokOpenParen
           then error $ errorMessage OpenParen
           else
        let (funcArgList, toks') = parseFunctionArgs [] (paren:toks)
            in
        case id of
             TokIdent funcName ->
                     (FuncCallNode funcName funcArgList, toks')
             _ -> error $ "Invalid function argument: " ++ show id


parseFunctionArgs :: [Tree] -> [Token] -> ([Tree], [Token])
parseFunctionArgs argList (first:second:toks)
        | first == TokCloseParen                       = (argList, second:toks)
        | first /= TokOpenParen && first /= TokComma   = error "Missing comma between arguments"
        | first == TokComma && second == TokCloseParen = error "Missing argument"
        | otherwise = case second of
                           TokCloseParen -> (argList, toks)
                           _             ->
                                   let (argTree, toks') = parseArgument (second:toks)
                                       in
                                   parseFunctionArgs (argList ++ [argTree]) toks'


parseArgument :: [Token] -> (Tree, [Token])
parseArgument toks =
        let (argTree, toks') = parseExpression toks
            in
        (ArgNode argTree, toks')


nullExpr :: [Token] -> (Tree, [Token])
nullExpr toks = (NullExprNode, toks)


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


validType :: Keyword -> Bool
validType kwd = kwd `elem` [Int]


isAssignment :: Operator -> Bool
isAssignment op = op `elem` [Assign,PlusAssign,MinusAssign,
                             MultiplyAssign,DivideAssign,ModuloAssign]


setType :: Token -> Token -> Type
setType (TokKeyword Int) (TokOp Multiply) = IntPointer
setType (TokKeyword Int) _                = IntVar
setType a                b                = error $ "unrecognised type: "
                                            ++ show a ++ " " ++ show b


data Error = SemiColon
           | OpenBrace
           | CloseBrace
           | OpenParen
           | CloseParen
           | TypeError
           | ParseError
           deriving Eq


errorMessage :: Error -> String
errorMessage err
    | err == SemiColon  = "Missing semicolon"
    | err == OpenBrace  = "Missing opening brace"
    | err == CloseBrace = "Missing closing brace"
    | err == OpenParen  = "Missing opening parenthesis"
    | err == CloseParen = "Missing closing parenthesis"
    | err == TypeError  = "Invalid type: "
    | err == ParseError = "Parse error on token: "
