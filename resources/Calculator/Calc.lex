
{
module Lab3.Calculator.Lexer
  ( -- * Lexer parser
    lexer

    -- * Token constructors
  , Token(..)
  )
where
import Data.Maybe (fromJust)
}

%lexername { myLexer }
%tokentype { Token }

%token

[0-9]+          { TokenNum . read }
\\+             { const TokenSum }
-               { const TokenSub }
\\*             { const TokenMul }
/               { const TokenDiv }
\\(             { const TokenOB }
\\)             { const TokenCB }
[ \t\r\n]+      { const TokenSP }

{
data Token
 = TokenSum
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenOB
 | TokenCB
 | TokenSP
 | TokenNum Double
 deriving (Show, Eq)

lexer :: String -> [Token]
lexer str = fst . fromJust $ (runParser myLexer str)
}