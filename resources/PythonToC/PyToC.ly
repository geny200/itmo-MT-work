{
module Interpreter.PythonToC.Parser
  ( -- * Function
    interpret
  )
where

import Interpreter.PythonToC.Lexer (lexer, Token(..), DataTree(..))
import Control.Lens ((^.), (.~), (%~))
import Data.Set (Set (..), empty, insert, toList)
import Utils (genericJoin, join)
import Control.Monad (unless)

}

%name parser
%tokentype { Token }

%lexername lexer
%attributetype  { MyData }
%attribute pos  { Int }
%attribute value  { DataTree }
%attribute vars { Set DataTree }

%token '+'      { TokenSum }
       '-'      { TokenSub }
       '*'      { TokenMul }
       '/'      { TokenDiv }
       '('      { TokenOB }
       ')'      { TokenCB }
       '='      { TokenEq }
       '.'      { TokenDot }
       s        { TokenSP }
       int      { TokenInt }
       while    { TokenWhile }
       if       { TokenIf }
       else     { TokenElse }
       print    { TokenPrint }
       input    { TokenInput }
       endl     { TokenEndl }
       tab      { TokenTab }
       name     { TokenName _; $$ :: value .~ (inside $1) }
       const    { TokenNum _; $$ :: value .~ (inside $1) }
%%

BLOCK :
      BLOCKTAIL                 { $$ :: value .~ (Block ($1^.value) ($1^.vars)) }

BLOCKTAIL :
      TAB ASSIGN endl BLOCKTAIL      { $4 :: vars .~ ($2^.vars);;
                                  $$ :: value .~ (BlockTail ($2^.value) ($4^.value));
                                        vars .~ ($4^.vars) }
    | TAB CONTROL endl BLOCKTAIL     { $$ :: value .~ (BlockTail ($2^.value) ($4^.value)) }
    |                           {}

E :   T '+' E     	    	    { $$ :: value .~ (BinOp "+" ($1^.value) ($3^.value)) }
    | T '-' E		            { $$ :: value .~ (BinOp "-" ($1^.value) ($3^.value)) }
    | T		        	        { $$ :: value .~ ($1^.value) }

T :   F '*' T	                { $$ :: value .~ (BinOp "*" ($1^.value) ($3^.value)) }
    | F '/' T	                { $$ :: value .~ (BinOp "/" ($1^.value) ($3^.value)) }
    | F			                { $$ :: value .~ ($1^.value) }

F :   NUM		                { $$ :: value .~ ($1^.value) }
    | BR                        { $$ :: value .~ ($1^.value) }

BR :  S '(' S E S ')' S         { $$ :: value .~ (Bracket ($4^.value)) }

NUM :
      S const S '.' S const S   { $$ :: value .~ (BinOp "." ($2^.value) ($6^.value)) }
    | S const S                 { $$ :: value .~ ($2^.value) }



CONTROL :
      while BR BLOCK            { $3 :: pos %~ (+1);;
                                  $$ :: value .~ (While ($2^.value) ($3^.value)) }

ASSIGN :
      name s '=' s E            { $$ :: value .~ (BinOp "=" ($1^.value) ($5^.value)); vars %~ insert ($1^.value) }
    | name s '=' s INT          { $5 :: value .~ $1^.value;;
                                  $$ :: value .~ (BinOp "=" ($1^.value) ($5^.value)); vars %~ insert ($1^.value) }
    | E                         { $$ :: value .~ ($1^.value) }

INT : int s '(' s input s '(' s ')' s ')' s
                                { $$ :: value %~ (Input) }
PRINT :
      print BR                  { $$ :: value .~ (Print ($1^.value)) }

TAB :
      tab TAB                   { $2 :: pos %~ (\x -> x - 1) }
    | S                         { where unless ($1^.pos == 0) (error "wrong tab") }

S : s | {}
{
instance Show DataTree where
    show (BinOp op l r) = (show l) ++ op ++ (show r)
    show (Const val) = val
    show (Bracket x) = "(" ++ (show x) ++ ")"
    show (While x y) = "while" ++ (show x) ++ (show y)
    show Nop = "Nop"
    show (BlockTail x y) = join "\n" [show x, show y]
    show (Block x var) =
        (if not . null $ var
        then "int " ++ (genericJoin ", " (toList var)) ++ "\n"
        else "")
        ++ show x
    show (Input x) = "scanf(\"%d\", &" ++ (show x) ++ ")"
    show (Print x) = "printf(\"%d\"," ++ (show x) ++ ")"

inside :: Token -> DataTree
inside (TokenNum x) = Const x
inside (TokenName x) = Const x

interpret :: String -> Maybe String
interpret str = show <$> (fst <$> (parser (SheolAttributes 0 Nop empty) str))

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
}