
{
module Test.Lexer
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
(               { const TokenOB }
)               { const TokenCB }

{
data Token
 = TokenSum
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenOB
 | TokenCB
 | TokenNum Integer
 deriving (Show, Eq)

lexer :: String -> [Token]
lexer str = fst . fromJust $ (runParser myLexer str)
}