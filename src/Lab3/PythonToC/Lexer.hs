-- | Before block                                               
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
                                                              
                                                                
import Lab4.Parser.Combinator (eof, regExp)                     
import Lab4.Parser.Parser (Parser (..), (<|>))                  
                                                                
-- parser produced by SheolLexer Version 1.0.0                  
                                                                
-- | Token parsers                                              
tokenParse1 = (TokenNum ) <$> regExp "^[0-9]+"

tokenParse2 = (const TokenSum ) <$> regExp "^\\+"

tokenParse3 = (const TokenSub ) <$> regExp "^-"

tokenParse4 = (const TokenMul ) <$> regExp "^\\*"

tokenParse5 = (const TokenDot ) <$> regExp "^\\."

tokenParse6 = (const TokenDiv ) <$> regExp "^/"

tokenParse7 = (const TokenMod ) <$> regExp "^%"

tokenParse8 = (const TokenOB ) <$> regExp "^\\("

tokenParse9 = (const TokenCB ) <$> regExp "^\\)"

tokenParse10 = (const TokenEq ) <$> regExp "^="

tokenParse11 = (const TokenColon ) <$> regExp "^:"

tokenParse12 = (const TokenInt ) <$> regExp "^int"

tokenParse13 = (const TokenWhile ) <$> regExp "^while"

tokenParse14 = (const TokenIf ) <$> regExp "^if"

tokenParse15 = (const TokenElse ) <$> regExp "^else"

tokenParse16 = (const TokenPrint ) <$> regExp "^print"

tokenParse17 = (const TokenInput ) <$> regExp "^input"

tokenParse18 = (const TokenSP ) <$> regExp "^[ \v\r\f]+"

tokenParse19 = (const TokenSemicolon ) <$> regExp "^;"

tokenParse20 = (const TokenEndl ) <$> regExp "^[\n]([ \t\v\r\f]*\n)*"

tokenParse21 = (const TokenTab ) <$> regExp "^[\t]"

tokenParse22 = (TokenName ) <$> regExp "^[a-zA-Z][a-zA-Z0-9]*"
                                                              
                                                                
-- | Union of all token parsers                                 
commonLexer :: Parser Char Token                                    
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8 <|> tokenParse9 <|> tokenParse10 <|> tokenParse11 <|> tokenParse12 <|> tokenParse13 <|> tokenParse14 <|> tokenParse15 <|> tokenParse16 <|> tokenParse17 <|> tokenParse18 <|> tokenParse19 <|> tokenParse20 <|> tokenParse21 <|> tokenParse22                                                
                                                                
-- | Generated lexer                                            
myLexer  :: Parser Char [Token ]                                          
myLexer  = ((:) <$> commonLexer <*> myLexer ) <|> (eof >> pure [])          
                                                                
-- | After block                                                
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
                                                              
