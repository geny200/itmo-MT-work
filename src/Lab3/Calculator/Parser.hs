{-# LANGUAGE TemplateHaskell #-}                                
-- | Before block                                               
module Lab3.Calculator.Parser
  ( -- * Function
    calculate
  )
where

import Lab3.Calculator.Lexer (lexer, Token(..))
import Control.Lens ((^.), (.~))
                                                              
                                                                
import Control.Applicative ((<|>))                              
import Lab4.Parser.Combinator (satisfy, nothing)                
import Lab4.Parser.Parser (Parser (..))                         
import Control.Lens (makeLenses, (&))                           
                                                                
-- parser produced by Sheol Version 1.0.0                       
data MyData  = SheolAttributes 
  { _value :: Integer                     
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
    return (cur)             
    where func (TokenSP ) = True  
          func _ = False
token8 cur =                       
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
    a1<- parserS (cur)
    a2<- token8 (cur)
    a3<- parserS (cur)                    
    return (cur & value .~ (a2^.value) )
  <|>
  do 
    a1<- parserS (cur)
    a2<- token5 (cur)
    a3<- parserS (cur)
    a4<- parserE (cur)
    a5<- parserS (cur)
    a6<- token6 (cur)
    a7<- parserS (cur)                    
    return (cur & value .~ (a4^.value) )                          
                          
parserS cur =                     
  do 
    a1<- token7 (cur)                    
    return (cur)
  <|>
  do                     
    return (cur)                          
                                                              
                                                                
-- | Generated parser                                           
parser x = (runParser ((^. value) <$> (parserE x))) . lexer                 
                                                                
-- | After block                                                
num :: Token -> Integer
num (TokenNum x) = x

calculate :: String ->Maybe Integer
calculate str = fst <$> (parser (SheolAttributes 0) str)

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
                                                              
