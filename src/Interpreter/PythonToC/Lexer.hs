-- | Before block                                               
module Interpreter.PythonToC.Lexer
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
                                                              
                                                                
import Generator.Parser.Combinator (eof, regExp)                
import Generator.Parser.Parser (Parser (..), (<|>))             
                                                                
-- | Token parsers                                              
tokenParse1 = (TokenNum ) <$> regExp "^[0-9]+"

tokenParse2 = (const TokenSum ) <$> regExp "^\\+"

tokenParse3 = (const TokenSub ) <$> regExp "^-"

tokenParse4 = (const TokenMul ) <$> regExp "^\\*"

tokenParse5 = (const TokenDot ) <$> regExp "^\\."

tokenParse6 = (const TokenDiv ) <$> regExp "^/"

tokenParse7 = (const TokenOB ) <$> regExp "^\\("

tokenParse8 = (const TokenCB ) <$> regExp "^\\)"

tokenParse9 = (const TokenEq ) <$> regExp "^="

tokenParse10 = (const TokenInt ) <$> regExp "^int"

tokenParse11 = (const TokenWhile ) <$> regExp "^while"

tokenParse12 = (const TokenIf ) <$> regExp "^if"

tokenParse13 = (const TokenElse ) <$> regExp "^else"

tokenParse14 = (const TokenPrint ) <$> regExp "^print"

tokenParse15 = (const TokenInput ) <$> regExp "^input"

tokenParse16 = (const TokenSP ) <$> regExp "^[ \v\r\f]+"

tokenParse17 = (const TokenEndl ) <$> regExp "^[\n;]([ \t\v\r\f]*[\n;])?"

tokenParse18 = (const TokenTab ) <$> regExp "^[\t]"

tokenParse19 = (TokenName ) <$> regExp "^[a-zA-Z][a-zA-Z0-9]*"
                                                              
                                                                
-- | Union of all token parsers                                 
commonLexer :: Parser Char Token                                    
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8 <|> tokenParse9 <|> tokenParse10 <|> tokenParse11 <|> tokenParse12 <|> tokenParse13 <|> tokenParse14 <|> tokenParse15 <|> tokenParse16 <|> tokenParse17 <|> tokenParse18 <|> tokenParse19                                                
                                                                
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
 | Nop
 deriving (Eq, Ord)

lexer :: String -> [Token]
lexer str = fst . fromJust $ (runParser myLexer str)
                                                              
