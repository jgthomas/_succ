
module PrintError.MessageSyntaxError (syntaxErrorMsg) where


import Lexer.LexTab                (LexDat (..))
import PrintError.PrintErrorTokens (PrintRange (..), buildLineMsg, buildTokMsg)
import Types.Error                 (SyntaxError (..))


syntaxErrorMsg :: SyntaxError -> (String, PrintRange)

syntaxErrorMsg (MissingToken t d) = (msg, mkRange d)
        where msg = unexpectedLexDatMsg d
                    ++ ", Expected "
                    ++ buildTokMsg t

syntaxErrorMsg (BadType d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Invalid type "
                    ++ buildTokMsg (tok d)

syntaxErrorMsg (UnexpectedLexDat d) = (msg, mkRange d)
        where msg = unexpectedLexDatMsg d

syntaxErrorMsg (NonValidIdentifier d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Invalid identifier "
                    ++ buildTokMsg (tok d)

syntaxErrorMsg (MissingKeyword kwd d) = (msg, mkRange d)
        where msg = buildLineMsg (line d)
                    ++ "Expected keyword " ++ show kwd

syntaxErrorMsg err = (show err, All)


mkRange :: LexDat -> PrintRange
mkRange d = Range (pred . line $ d) (succ . line $ d)


unexpectedLexDatMsg :: LexDat -> String
unexpectedLexDatMsg d =
        buildLineMsg (line d)
        ++ "Unexpected token "
        ++ buildTokMsg (tok d)
