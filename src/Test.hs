-- | Before block                                               
module Test
  ( -- * Lexer parser
    myLexer
  )
where
                                                              
                                                                
import Control.Applicative ((<|>))                              
import Parser.Combinator (eof, regExp)                          
import Parser.Parser (Parser (..))                              
                                                                
-- | Token parsers                                              
tokenParse1 :: Parser Char Token 
tokenParse1 = (TDigit . read ) <$> regExp "^[0-9]+"

tokenParse2 :: Parser Char Token 
tokenParse2 = (const TIf ) <$> regExp "^if"

tokenParse3 :: Parser Char Token 
tokenParse3 = (const TWhile ) <$> regExp "^while"

                                                              
                                                                
-- | Union of all token parsers                                 
commonLexer :: Parser Char Token                                     
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3                                                
                                                                
-- | Generated lexer                                            
myLexer  :: Parser Char [Token ]                                          
myLexer  = ((:) <$> commonLexer <*> myLexer ) <|> (eof >> pure [])          
                                                                
-- | After block                                                
data Token
 = TDigit Integer
 | TIf
 | TWhile
 deriving Show
                                                              
