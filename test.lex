
%%lexername { myLexer }
%%tokentype { Token }
%%tokens

[0-9]+          { TDigit . read }
if              { const TIf }
while           { const TWhile }

{
data Token
 = TDigit Integer
 | TIf
 | TWhile
 deriving Show
}