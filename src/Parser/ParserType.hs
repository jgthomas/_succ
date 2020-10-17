-- |
-- Module       : ParserType
-- Description  : Parses type
--
-- Determines the type of a function, declaration or other typed construct.
module Parser.ParserType
  ( parseType,
  )
where

import Parser.ParState (ParserState, throwError)
import Types.Error
  ( CompilerError (ParserError, SyntaxError),
    ParserError (..),
    SyntaxError (..),
  )
import Types.Tokens
import Types.Type (Type (..))

-- | Determines the type of a function or variable
parseType :: [Token] -> ParserState Type
parseType (Keyword Int _ : rest) = parseIntType rest
parseType (token : _) = throwError $ SyntaxError (BadType token)
parseType tokens = throwError $ ParserError (LexDataError tokens)

parseIntType :: [Token] -> ParserState Type
parseIntType tokens
  | isIntPointer tokens = pure IntPointer
  | isIntArray tokens = pure IntArray
  | otherwise = pure IntVar

isIntArray :: [Token] -> Bool
isIntArray
  ( _ : OpenBracket OpenSqBracket _ :
      CloseBracket CloseSqBracket _ :
      _
    ) = True
isIntArray
  ( _ : OpenBracket OpenSqBracket _ :
      ConstInt _ _ :
      CloseBracket CloseSqBracket _ :
      _
    ) = True
isIntArray _ = False

isIntPointer :: [Token] -> Bool
isIntPointer (OpTok Asterisk _ : _) = True
isIntPointer _ = False
