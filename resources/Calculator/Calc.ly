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
%attribute value { Double }

%token '+'      { TokenSum }
       '-'      { TokenSub }
       '*'      { TokenMul }
       '/'      { TokenDiv }
       '('      { TokenOB }
       ')'      { TokenCB }
       s        { TokenSP }
       const    { TokenNum _; $$ :: value .~ (num $1) }
%%

E :   T ET 	    	    { $$ :: value .~ ($1^.value + $2^.value) }

ET :    '+' T ET	   	{ $$ :: value .~ ($2^.value + $3^.value) }
      | '-' T ET	    { $$ :: value .~ (-$2^.value + $3^.value) }
      | 			    { }

T :    P TP	            { $$ :: value .~ ($1^.value * $2^.value) }

TP :   '*' P TP	        { $$ :: value .~ ($2^.value * $3^.value) }
     | '/' P TP	        { $$ :: value .~ ((del ($2^.value)) * $3^.value) }
     |  			    { $$ :: value .~ 1 }

P :    F '*' '*' P	    { $$ :: value .~ (($1^.value) ** ($4^.value)) }
     | F			    { $$ :: value .~ ($1^.value) }

F :    S const S		{ $$ :: value .~ ($2^.value) }
	 | S '(' S E S ')' S{ $$ :: value .~ ($4^.value) }

S : s |

{
num :: Token -> Double
num (TokenNum x) = x

calculate :: String -> Maybe Double
calculate str = fst <$> (parser (SheolAttributes (0 :: Double)) str)

del :: Double -> Double
del x = 1 / x

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
}