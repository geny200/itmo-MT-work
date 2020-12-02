{
module Test.Parser
  ( -- * Function
    calculate
  )
where

import Test.Lexer (lexer, Token(..))
import Control.Lens ((^.), (.~))
}

%name parser
%tokentype { Token }

%lexername lexer
%attributetype { MyData }
%attribute pos { Int }
%attribute value { Integer }

%token '+'      { TokenSum }
       '-'      { TokenSub }
       '*'      { TokenMul }
       '/'      { TokenDiv }
       '('      { TokenOB }
       ')'      { TokenCB }
       const    { TokenNum _; $$ :: value .~ (num $1) }
%%

E :     T '+' E	    	{ $$ :: value .~ ($1^.value + $3^.value) }
      | T '-' E		    { $$ :: value .~ ($1^.value + $3^.value) }
      | T			    { $$ :: value .~ ($1^.value) }

T :    F '*' T	        { $$ :: value .~ ($1^.value * $3^.value) }
     | F '/' T	        { $$ :: value .~ ($1^.value `div` $3^.value) }
     | F			    { $$ :: value .~ ($1^.value) }

F :    const			{ $$ :: value .~ ($1^.value) }
	 | '(' E ')'		{ $$ :: value .~ ($2^.value) }



{
num :: Token -> Integer
num (TokenNum x) = x

calculate :: String ->Maybe Integer
calculate str = fst <$> (parser (SheolAttributes 0 0) str)

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
}