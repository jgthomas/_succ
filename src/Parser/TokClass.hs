
module Parser.TokClass where


import Types.Tokens


data OpTokType = LogicalOR
               | LogicalAND
               | Factor
               | Term
               | Assign
               | Equality
               | Relational
               | PostPosition
               | BitwiseXOR
               | BitwiseAND
               | BitwiseOR
               | Shift


isAssign :: OpTok -> Bool
isAssign op = op `elem` kind Assign


isPostPos :: OpTok -> Bool
isPostPos op = op `elem` kind PostPosition


kind :: OpTokType -> [OpTok]
kind tokTyp =
        case tokTyp of
             LogicalOR    -> [PipePipe]
             LogicalAND   -> [AmpAmp]
             Factor       -> [Asterisk,
                              Backslash,
                              Percent]
             Term         -> [PlusSign,
                              MinusSign]
             Equality     -> [EqualEqual,
                              BangEqual]
             Relational   -> [RightArrow,
                              LeftArrow,
                              RightArrowEqual,
                              LeftArrowEqual]
             Assign       -> [EqualSign,
                              PlusEqual,
                              MinusEqual,
                              AsteriskEqual,
                              BackslashEqual,
                              PercentEqual,
                              AmpEqual,
                              CaretEqual,
                              PipeEqual,
                              DoubleLArrowEqual,
                              DoubleRArrowEqual]
             PostPosition -> [PlusPlus,
                              MinusMinus]
             BitwiseXOR   -> [Caret]
             BitwiseAND   -> [Ampersand]
             BitwiseOR    -> [Pipe]
             Shift        -> [DoubleLeftArrow,
                              DoubleRightArrow]
