{-|
Module       : ParserSequence
Description  : Parses repeating sequences

Parses sequences like function arguments.
-}
module Parser.ParserSequence (parseBracketedSeq) where


import Parser.ParState   (ParserState, throwError)
import Parser.TokConsume (consumeTok)
import Types.AST         (Tree)
import Types.Error       (CompilerError (ParserError, SyntaxError),
                          ParserError (..), SyntaxError (..))
import Types.Tokens


-- | Parse a bracketed sequence of elements
parseBracketedSeq :: [Tree]
                  -> [Token]
                  -> ([Tree] -> [Token] -> ParserState ([Tree], [Token]))
                  -> ParserState ([Tree], [Token])

parseBracketedSeq _ [] _ = throwError $ ParserError (LexDataError [])

parseBracketedSeq trees tokens@(OpenBracket _ _:CloseBracket _ _:_) _ = do
                                      tokens' <- consumeTok tokens
                                      pure (reverse trees, tokens')

parseBracketedSeq trees tokens@(CloseBracket _ _:_) _ = pure (reverse trees, tokens)

parseBracketedSeq _ (token@(Separator Comma _):CloseBracket _ _:_) _ =
        throwError $ SyntaxError (UnexpectedLexDat token)

parseBracketedSeq trees (OpenBracket _ _:rest) f   = f trees rest

parseBracketedSeq trees (Separator Comma _:rest) f = f trees rest

parseBracketedSeq _ (token:_) _ = throwError $ SyntaxError (UnexpectedLexDat token)
