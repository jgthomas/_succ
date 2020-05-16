{-|
Module       : ParserType
Description  : Parses type

Determines the type of a function, declaration or other typed construct.
-}
module Parser.ParserType (parseType) where


import Parser.ParState (ParserState, throwError)
import Types.Error     (CompilerError (ParserError, SyntaxError),
                        ParserError (..), SyntaxError (..))
import Types.LexDat    (LexDat (..))
import Types.Tokens
import Types.Type      (Type (..))


-- | Determines the type of a function or variable
parseType :: [LexDat] -> ParserState Type
parseType (LexDat{tok=Keyword Int}:rest) = parseIntType rest
parseType (a:_)   = throwError $ SyntaxError (BadType a)
parseType lexData = throwError $ ParserError (LexDataError lexData)


parseIntType :: [LexDat] -> ParserState Type
parseIntType lexData
        | isIntPointer lexData = pure IntPointer
        | isIntArray lexData   = pure IntArray
        | otherwise            = pure IntVar


isIntArray :: [LexDat] -> Bool
isIntArray (_:LexDat{tok=OpenBracket OpenSqBracket}:
              LexDat{tok=CloseBracket CloseSqBracket}:_) = True
isIntArray (_:LexDat{tok=OpenBracket OpenSqBracket}:
              LexDat{tok=ConstInt _}:
              LexDat{tok=CloseBracket CloseSqBracket}:_) = True
isIntArray _                                             = False


isIntPointer :: [LexDat] -> Bool
isIntPointer (LexDat{tok=OpTok Asterisk}:_) = True
isIntPointer _                              = False
