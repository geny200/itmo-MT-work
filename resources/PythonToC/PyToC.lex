
{
module Lab3.PythonToC.Lexer
  ( -- * Lexer parser
    lexer

    -- * Token constructors
  , Token(..)

    -- * DataTree constructors
  , DataTree(..)
  )
where
import Data.Maybe (fromJust)
import Data.Set (Set (..), toList)
}

%lexername { myLexer }
%tokentype { Token }

%token

[0-9]+              { TokenNum }
\\+                 { const TokenSum }
-                   { const TokenSub }
\\*                 { const TokenMul }
\\.                 { const TokenDot }
/                   { const TokenDiv }
%                   { const TokenMod }
\\(                 { const TokenOB }
\\)                 { const TokenCB }
=                   { const TokenEq }
:                   { const TokenColon }
int                 { const TokenInt }
while               { const TokenWhile }
if                  { const TokenIf }
else                { const TokenElse }
print               { const TokenPrint }
input               { const TokenInput }
[ \v\r\f]+          { const TokenSP }
;                   { const TokenSemicolon }
[\n]([ \t\v\r\f]*\n)* { const TokenEndl }
[\t]                { const TokenTab }
[a-zA-Z][a-zA-Z0-9]*{ TokenName }

{
data Token
 = TokenSum
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenEq
 | TokenMod
 | TokenOB
 | TokenCB
 | TokenInt
 | TokenWhile
 | TokenIf
 | TokenElse
 | TokenPrint
 | TokenInput
 | TokenSP
 | TokenEndl
 | TokenTab
 | TokenDot
 | TokenColon
 | TokenSemicolon
 | TokenName String
 | TokenNum String
 deriving (Show, Eq)

data DataTree
 = While DataTree DataTree
 | BinOp String DataTree DataTree
 | Const String
 | Bracket DataTree
 | Input DataTree
 | Print DataTree
 | Block DataTree (Set DataTree)
 | BlockTail DataTree DataTree
 | Tail DataTree
 | Main DataTree
 | Expr DataTree
 | Nop
 deriving (Eq, Ord)

lexer :: String -> [Token]
lexer str = fst . fromJust $ (runParser myLexer str)
}