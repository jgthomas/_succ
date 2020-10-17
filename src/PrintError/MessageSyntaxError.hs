module PrintError.MessageSyntaxError
  ( syntaxErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..))
import qualified PrintError.PrintErrorTokens as PrintErrorTokens
  ( buildLineMsg,
    buildTokMsg,
  )
import Types.Error (SyntaxError (..))
import Types.Tokens

syntaxErrorMsg :: SyntaxError -> (String, PrintRange)
syntaxErrorMsg (MissingToken t1 t2) = (msg, mkRange t2)
  where
    msg =
      unexpectedLexDatMsg t2
        ++ ", Expected "
        ++ PrintErrorTokens.buildTokMsg t1
syntaxErrorMsg (BadType d) = (msg, mkRange d)
  where
    msg =
      buildLineMsg d
        ++ "Invalid type "
        ++ PrintErrorTokens.buildTokMsg d
syntaxErrorMsg (UnexpectedLexDat d) = (msg, mkRange d)
  where
    msg = unexpectedLexDatMsg d
syntaxErrorMsg (NonValidIdentifier d) = (msg, mkRange d)
  where
    msg =
      buildLineMsg d
        ++ "Invalid identifier "
        ++ PrintErrorTokens.buildTokMsg d
syntaxErrorMsg err = (show err, All)

mkRange :: Token -> PrintRange
mkRange token =
  let dat = tokenData token
   in Range (pred . line $ dat) (succ . line $ dat)

buildLineMsg :: Token -> String
buildLineMsg token = PrintErrorTokens.buildLineMsg . line . tokenData $ token

unexpectedLexDatMsg :: Token -> String
unexpectedLexDatMsg token =
  buildLineMsg token
    ++ "Unexpected token "
    ++ PrintErrorTokens.buildTokMsg token
