-- | Before block                                               
module Interpreter.Calculator.Lexer
  ( -- * Lexer parser
    lexer

    -- * Token constructors
  , Token(..)
  )
where
import Data.Maybe (fromJust)
                                                              
                                                                
import Generator.Parser.Combinator (eof, regExp)                
import Generator.Parser.Parser (Parser (..), (<|>))             
                                                                
-- | Token parsers                                              
tokenParse1 = (TokenNum . read ) <$> regExp "^[0-9]+"

tokenParse2 = (const TokenSum ) <$> regExp "^\\+"

tokenParse3 = (const TokenSub ) <$> regExp "^-"

tokenParse4 = (const TokenMul ) <$> regExp "^\\*"

tokenParse5 = (const TokenDiv ) <$> regExp "^/"

tokenParse6 = (const TokenOB ) <$> regExp "^\\("

tokenParse7 = (const TokenCB ) <$> regExp "^\\)"

tokenParse8 = (const TokenSP ) <$> regExp "^[ \t\r\n]+"
                                                              
                                                                
-- | Union of all token parsers                                 
commonLexer :: Parser Char Token                                    
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8                                                
                                                                
-- | Generated lexer                                            
myLexer  :: Parser Char [Token ]                                          
myLexer  = ((:) <$> commonLexer <*> myLexer ) <|> (eof >> pure [])          
                                                                
-- | After block                                                
data Token
 = TokenSum
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenOB
 | TokenCB
 | TokenSP
 | TokenNum Integer
 deriving (Show, Eq)

lexer :: String -> [Token]
lexer str = fst . fromJust $ (runParser myLexer str)
                                                              
