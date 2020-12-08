{
module Lab3.Calculator.Parser
  ( -- * Function
    calculate
  )
where

import Lab3.Calculator.Lexer (lexer, Token(..))
import Control.Lens ((^.), (.~))
}

%name parser
%tokentype { Token }

%lexername lexer
%attributetype { MyData }
%attribute value { Integer }

%token '+'      { TokenSum }
       '-'      { TokenSub }
       '*'      { TokenMul }
       '/'      { TokenDiv }
       '('      { TokenOB }
       ')'      { TokenCB }
       s        { TokenSP }
       const    { TokenNum _; $$ :: value .~ (num $1) }
%%

E :     T '+' E	    	{ $$ :: value .~ ($1^.value + $3^.value) }
      | T '-' E		    { $$ :: value .~ ($1^.value + $3^.value) }
      | T			    { $$ :: value .~ ($1^.value) }

T :    F '*' T	        { $$ :: value .~ ($1^.value * $3^.value) }
     | F '/' T	        { $$ :: value .~ ($1^.value `div` $3^.value) }
     | F			    { $$ :: value .~ ($1^.value) }

F :    S const S		{ $$ :: value .~ ($2^.value) }
	 | S '(' S E S ')' S{ $$ :: value .~ ($4^.value) }

S : s |

{
num :: Token -> Integer
num (TokenNum x) = x

calculate :: String ->Maybe Integer
calculate str = fst <$> (parser (SheolAttributes 0) str)

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
}