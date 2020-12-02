{-# LANGUAGE TemplateHaskell #-}                                
-- | Before block                                               
module Test.Parser
  ( -- * Function
    calculate
  )
where

import Test.Lexer (lexer, Token(..))
import Control.Lens ((^.), (.~))
                                                              
                                                                
import Control.Applicative ((<|>))                              
import Parser.Combinator (satisfy)                              
import Parser.Parser (Parser (..))                              
import Control.Lens (makeLenses, (&))                           
                                                                
-- parser produced by Sheol Version 1.0.0                       
data MyData  = SheolAttributes 
  { _pos :: Int 
  , _value :: Integer                     
  }                       
                          
makeLenses ''MyData           
                                                              
                                                                
token1 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenSum ) = True  
          func _ = False
token2 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenSub ) = True  
          func _ = False
token3 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenMul ) = True  
          func _ = False
token4 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenDiv ) = True  
          func _ = False
token5 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenOB ) = True  
          func _ = False
token6 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenCB ) = True  
          func _ = False
token7 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur & value .~ (num a1) )             
    where func (TokenNum _) = True  
          func _ = False                                                              
                                                                
-- | Generated parsers                                          
errorEndPoint :: Parser Token a                                 
errorEndPoint = Parser sheolError                               
                                                                
                          
parserE cur =                     
  do 
    a1<- parserT (cur)
    a2<- token1 (cur)
    a3<- parserE (cur)                    
    return (cur & value .~ (a1^.value + a3^.value) )
  <|>
  do 
    a1<- parserT (cur)
    a2<- token2 (cur)
    a3<- parserE (cur)                    
    return (cur & value .~ (a1^.value + a3^.value) )
  <|>
  do 
    a1<- parserT (cur)                    
    return (cur & value .~ (a1^.value) )                          
                          
parserT cur =                     
  do 
    a1<- parserF (cur)
    a2<- token3 (cur)
    a3<- parserT (cur)                    
    return (cur & value .~ (a1^.value * a3^.value) )
  <|>
  do 
    a1<- parserF (cur)
    a2<- token4 (cur)
    a3<- parserT (cur)                    
    return (cur & value .~ (a1^.value `div` a3^.value) )
  <|>
  do 
    a1<- parserF (cur)                    
    return (cur & value .~ (a1^.value) )                          
                          
parserF cur =                     
  do 
    a1<- token7 (cur)                    
    return (cur & value .~ (a1^.value) )
  <|>
  do 
    a1<- token5 (cur)
    a2<- parserE (cur)
    a3<- token6 (cur)                    
    return (cur & value .~ (a2^.value) )                          
                                                              
                                                                
-- | Generated parser                                           
                                                              
parser x = (runParser ((^. value) <$> (parserE x))) . lexer                       
                                                                
-- | After block                                                
num :: Token -> Integer
num (TokenNum x) = x

calculate :: String ->Maybe Integer
calculate str = fst <$> (parser (SheolAttributes 0 0) str)

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
                                                              
