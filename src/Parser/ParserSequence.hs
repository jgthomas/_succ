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
--import Types.LexDat      (LexDat (..))
import Types.Tokens


-- | Parse a bracketed sequence of elements
parseBracketedSeq :: [Tree]
                  -> [Token]
                  -> ([Tree] -> [Token] -> ParserState ([Tree], [Token]))
                  -> ParserState ([Tree], [Token])
parseBracketedSeq _ [] _ = throwError $ ParserError (LexDataError [])
parseBracketedSeq xs tokens@(OpenBracket _ _:CloseBracket _ _:_) _ = do
                                      tokens' <- consumeTok tokens
                                      pure (reverse xs, tokens')
parseBracketedSeq xs tokens@(CloseBracket _ _:_) _ = pure (reverse xs, tokens)
parseBracketedSeq _ (d@(Comma _):CloseBracket _ _:_) _ =
        throwError $ SyntaxError (UnexpectedLexDat d)
parseBracketedSeq xs (OpenBracket _ _:rest) f = f xs rest
parseBracketedSeq xs (Comma _:rest) f         = f xs rest
parseBracketedSeq _ (a:_) _ = throwError $ SyntaxError (UnexpectedLexDat a)
