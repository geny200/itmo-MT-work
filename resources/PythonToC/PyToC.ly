{
module Lab3.PythonToC.Parser
  ( -- * Function
    interpret
  )
where

import Lab3.PythonToC.Lexer (lexer, Token(..), DataTree(..))
import Control.Lens ((^.), (.~), (%~))
import Data.Set (Set (..), difference, empty, insert, toList)
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
       ':'      { TokenColon }
       SCol     { TokenSemicolon }
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

PROGRAM :
    BLOCK                       { $$ :: value .~ (Main ($1^.value)) }

BLOCK :
      BLOCKTAIL S               { $1 :: vars .~  (empty);;
                                  $$ :: value .~ (Block ($1^.value) (difference ($1^.vars) ($2^.vars))) }
    | ANYEXPR endl BLOCKTAIL    { $1 :: vars .~  (empty);;
                                  $3 :: vars .~  ($1^.vars);;
                                  $$ :: value .~ (Block (BlockTail ($1^.value) ($3^.value)) (difference ($2^.vars) ($3^.vars))) }
    | ANYEXPR S                 { $1 :: vars .~  (empty);;
                                  $$ :: value .~ (Block (Tail ($1^.value)) (difference ($1^.vars) ($2^.vars))) }

BLOCKTAIL :
      TAB ANYEXPR endl BLOCKTAIL{ $4 :: vars .~ ($2^.vars);;
                                  $$ :: value .~ (BlockTail ($2^.value) ($4^.value));
                                        vars .~ ($4^.vars) }
    | TAB CONTROL endl BLOCKTAIL{ $$ :: value .~ (BlockTail ($2^.value) ($4^.value));
                                        vars .~ ($4^.vars) }
    | TAB CONTROL               { $$ :: value .~ ($2^.value) }
    | S endl BLOCKTAIL          { $$ :: value .~ ($3^.value); vars .~ ($3^.vars) }
    | TAB ANYEXPR               { $$ :: value .~ ($2^.value);
                                        vars .~ ($2^.vars)}

E :   T '+' E     	    	    { $$ :: value .~ (BinOp "+" ($1^.value) ($3^.value)) }
    | T '-' E		            { $$ :: value .~ (BinOp "-" ($1^.value) ($3^.value)) }
    | T		        	        { $$ :: value .~ ($1^.value) }

T :   F '*' T	                { $$ :: value .~ (BinOp "*" ($1^.value) ($3^.value)) }
    | F '/' T	                { $$ :: value .~ (BinOp "/" ($1^.value) ($3^.value)) }
    | F			                { $$ :: value .~ ($1^.value) }

F :   NUM		                { $$ :: value .~ ($1^.value) }
    | BR                        { $$ :: value .~ ($1^.value) }
    | S name S                  { $$ :: value .~ ($2^.value) }

BR :  S '(' S E S ')' S         { $$ :: value .~ (Bracket ($4^.value)) }

NUM :
      S const S '.' S const S   { $$ :: value .~ (BinOp "." ($2^.value) ($6^.value)) }
    | S const S                 { $$ :: value .~ ($2^.value) }

CONTROL :
      while E S ':' S BLOCK     { $6 :: pos %~ (+1);;
                                  $$ :: value .~ (While ($2^.value) ($6^.value)) }
ANYEXPR :
      ASSIGN S SCol S ANYEXPR   { $5 :: vars .~ ($1^.vars);;
                                  $$ :: value .~ (BlockTail ($1^.value) ($5^.value));
                                        vars .~ ($5^.vars) }
    | ASSIGN                    { $$ :: value .~ ($1^.value);
                                        vars .~ ($1^.vars) }
    |                           { }

ASSIGN :
      name S '=' S E            { $$ :: value .~ (BinOp "=" ($1^.value) ($5^.value)); vars %~ insert ($1^.value) }
    | name S '=' S INT          { $5 :: value .~ $1^.value;;
                                  $$ :: value .~ ($5^.value); vars %~ insert ($1^.value) }
    | E                         { $$ :: value .~ ($1^.value) }

INT : int S '(' S input S '(' S ')' S ')' S
                                { $$ :: value %~ (Input) }
PRINT :
      print BR                  { $$ :: value .~ (Print ($1^.value)) }

TAB :
      tab TAB                   { $2 :: pos %~ (\x -> x - 1) }
    | S                         { where unless ($1^.pos == 0) nothing }

S : s | {}
{
instance Show DataTree where
    show (BinOp op l r) = join (" " ++ op ++ " ") [show l, show r]
    show (Const val) = val
    show (Bracket x) = " (" ++ (show x) ++ ") "
    show (While x y) = "while (" ++ (show x) ++ ") {\n" ++ (show y) ++ "\n\125"
    show Nop = []
    show (BlockTail x y) = join ";\n" [show x, show y]
    show (Tail x) = show x ++ ";\n"
    show (Block x var) =
        (if not . null $ var
        then "int " ++ (genericJoin ", " (toList var)) ++ ";\n"
        else "")
        ++ show x
    show (Input x) = "scanf(\"%d\", &" ++ (show x) ++ ")"
    show (Print x) = "printf(\"%d\"," ++ (show x) ++ ")"
    show (Main x) = "int main() {\n" ++ (show x) ++ "\n\treturn 0; \n \125"

inside :: Token -> DataTree
inside (TokenNum x) = Const x
inside (TokenName x) = Const x

interpret :: String -> Maybe String
interpret str = show <$> (fst <$> (parser (SheolAttributes 0 Nop empty) str))

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
}