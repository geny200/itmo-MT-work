{-# LANGUAGE TemplateHaskell #-}                                
-- | Before block                                               
module Lab3.PythonToC.Parser
  ( -- * Function
    interpret
  )
where

import Lab3.PythonToC.Lexer (lexer, Token(..), DataTree(..))
import Control.Lens ((^.), (.~), (%~))
import Data.Set (Set (..), difference, empty, insert, toList)
import Utils (genericJoin, replace, join)
import Control.Monad (unless)
                                                              
                                                                
import Control.Applicative ((<|>))                              
import Lab4.Parser.Combinator (satisfy, nothing)                
import Lab4.Parser.Parser (Parser (..))                         
import Control.Lens (makeLenses, (&))                           
                                                                
-- parser produced by Sheol Version 1.0.0                       
data MyData  = SheolAttributes 
  { _pos :: Int 
  , _value :: DataTree 
  , _vars :: Set DataTree                     
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
    where func (TokenMod ) = True  
          func _ = False
token6 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenOB ) = True  
          func _ = False
token7 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenCB ) = True  
          func _ = False
token8 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenEq ) = True  
          func _ = False
token9 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenDot ) = True  
          func _ = False
token10 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenColon ) = True  
          func _ = False
token11 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenSemicolon ) = True  
          func _ = False
token12 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenSP ) = True  
          func _ = False
token13 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenInt ) = True  
          func _ = False
token14 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenWhile ) = True  
          func _ = False
token15 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenIf ) = True  
          func _ = False
token16 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenElse ) = True  
          func _ = False
token17 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenPrint ) = True  
          func _ = False
token18 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenInput ) = True  
          func _ = False
token19 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenEndl ) = True  
          func _ = False
token20 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur)             
    where func (TokenTab ) = True  
          func _ = False
token21 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur & value .~ (inside a1) )             
    where func (TokenName _) = True  
          func _ = False
token22 cur =                       
  do                        
    a1 <- satisfy func      
    return (cur & value .~ (inside a1) )             
    where func (TokenNum _) = True  
          func _ = False                                                              
                                                                
-- | Generated parsers                                          
errorEndPoint :: Parser Token a                                 
errorEndPoint = Parser sheolError                               
                                                                
                          
parserPROGRAM cur =                     
  do 
    a1<- parserBLOCK (cur)                    
    return (cur & value .~ (Main (a1^.value)) )                          
                          
parserBLOCK cur =                     
  do 
    a1<- parserBLOCKTAIL (cur)
    a2<- parserS (cur)                    
    return (cur & value .~ Block (a1^.value) (difference (a1^.vars) (a2^.vars)) )
  <|>
  do 
    a1<- parserANYEXPR (cur)
    a2<- token19 (cur)
    a3<- parserBLOCKTAIL (cur & vars .~  (a1^.vars))                    
    return (cur & value .~ Block (BlockTail (a1^.value) (a3^.value)) (difference (a3^.vars) (a2^.vars)) )
  <|>
  do 
    a1<- parserANYEXPR (cur)
    a2<- parserS (cur)                    
    return (cur & value .~ Block (a1^.value) (difference (a1^.vars) (a2^.vars)) )                          
                          
parserBLOCKTAIL cur =                     
  do 
    a1<- parserTAB (cur)
    a2<- parserANYEXPR (cur)
    a3<- token19 (cur)
    a4<- parserBLOCKTAIL (cur & vars .~ (a2^.vars))                    
    return (cur & value .~ BlockTail (a2^.value) (a4^.value) & vars .~ (a4^.vars) )
  <|>
  do 
    a1<- parserTAB (cur)
    a2<- parserCONTROL (cur)
    a3<- token19 (cur)
    a4<- parserBLOCKTAIL (cur)                    
    return (cur & value .~ BlockTail (a2^.value) (a4^.value) & vars .~ (a4^.vars) )
  <|>
  do 
    a1<- parserTAB (cur)
    a2<- parserCONTROL (cur)                    
    return (cur & value .~ (a2^.value) )
  <|>
  do 
    a1<- parserS (cur)
    a2<- token19 (cur)
    a3<- parserBLOCKTAIL (cur)                    
    return (cur & value .~ (a3^.value) & vars .~ (a3^.vars) )
  <|>
  do 
    a1<- parserTAB (cur)
    a2<- parserANYEXPR (cur)                    
    return (cur & value .~ (a2^.value) & vars .~ (a2^.vars))                          
                          
parserE cur =                     
  do 
    a1<- parserT (cur)
    a2<- token1 (cur)
    a3<- parserE (cur)                    
    return (cur & value .~ BinOp "+" (a1^.value) (a3^.value) )
  <|>
  do 
    a1<- parserT (cur)
    a2<- token2 (cur)
    a3<- parserE (cur)                    
    return (cur & value .~ BinOp "-" (a1^.value) (a3^.value) )
  <|>
  do 
    a1<- parserT (cur)                    
    return (cur & value .~ (a1^.value) )                          
                          
parserT cur =                     
  do 
    a1<- parserF (cur)
    a2<- token3 (cur)
    a3<- parserT (cur)                    
    return (cur & value .~ BinOp "*" (a1^.value) (a3^.value) )
  <|>
  do 
    a1<- parserF (cur)
    a2<- token4 (cur)
    a3<- parserT (cur)                    
    return (cur & value .~ BinOp "/" (a1^.value) (a3^.value) )
  <|>
  do 
    a1<- parserF (cur)
    a2<- token5 (cur)
    a3<- parserT (cur)                    
    return (cur & value .~ BinOp "%" (a1^.value) (a3^.value) )
  <|>
  do 
    a1<- parserF (cur)                    
    return (cur & value .~ (a1^.value) )                          
                          
parserF cur =                     
  do 
    a1<- parserNUM (cur)                    
    return (cur & value .~ (a1^.value) )
  <|>
  do 
    a1<- parserBR (cur)                    
    return (cur & value .~ (a1^.value) )
  <|>
  do 
    a1<- parserS (cur)
    a2<- token21 (cur)
    a3<- parserS (cur)                    
    return (cur & value .~ (a2^.value) )                          
                          
parserBR cur =                     
  do 
    a1<- parserS (cur)
    a2<- token6 (cur)
    a3<- parserS (cur)
    a4<- parserE (cur)
    a5<- parserS (cur)
    a6<- token7 (cur)
    a7<- parserS (cur)                    
    return (cur & value .~ Bracket (a4^.value) )                          
                          
parserNUM cur =                     
  do 
    a1<- parserS (cur)
    a2<- token22 (cur)
    a3<- parserS (cur)
    a4<- token9 (cur)
    a5<- parserS (cur)
    a6<- token22 (cur)
    a7<- parserS (cur)                    
    return (cur & value .~ BinOp "." (a2^.value) (a6^.value) )
  <|>
  do 
    a1<- parserS (cur)
    a2<- token22 (cur)
    a3<- parserS (cur)                    
    return (cur & value .~ (a2^.value) )                          
                          
parserCONTROL cur =                     
  do 
    a1<- token14 (cur)
    a2<- parserE (cur)
    a3<- parserS (cur)
    a4<- token10 (cur)
    a5<- parserS (cur)
    a6<- parserBLOCK (cur & pos %~ (+1))                    
    return (cur & value .~ While (a2^.value) (a6^.value) )
  <|>
  do 
    a1<- token15 (cur)
    a2<- parserE (cur)
    a3<- parserS (cur)
    a4<- token10 (cur)
    a5<- parserS (cur)
    a6<- parserBLOCK (cur & pos %~ (+1))
    a7<- parserELSE (cur)                    
    return (cur & value .~ If (a2^.value) (a6^.value) (a7^.value) )                          
                          
parserELSE cur =                     
  do 
    a1<- token19 (cur)
    a2<- parserTAB (cur)
    a3<- token16 (cur)
    a4<- parserS (cur)
    a5<- token10 (cur)
    a6<- parserS (cur)
    a7<- parserBLOCK (cur & pos %~ (+1))                    
    return (cur & value .~ (a7^.value) )
  <|>
  do                     
    return (cur)                          
                          
parserANYEXPR cur =                     
  do 
    a1<- parserASSIGN (cur)
    a2<- parserS (cur)
    a3<- token11 (cur)
    a4<- parserS (cur)
    a5<- parserANYEXPR (cur & vars .~ (a1^.vars))                    
    return (cur & value .~ BlockTail (a1^.value) (a5^.value) & vars .~ (a5^.vars) )
  <|>
  do 
    a1<- parserASSIGN (cur)                    
    return (cur & value .~ (a1^.value) & vars .~ (a1^.vars) )
  <|>
  do                     
    return (cur)                          
                          
parserASSIGN cur =                     
  do 
    a1<- token21 (cur)
    a2<- parserS (cur)
    a3<- token8 (cur)
    a4<- parserS (cur)
    a5<- parserE (cur)                    
    return (cur & value .~ Expr (BinOp "=" (a1^.value) (a5^.value)) & vars %~ insert (a1^.value) )
  <|>
  do 
    a1<- token21 (cur)
    a2<- parserS (cur)
    a3<- token8 (cur)
    a4<- parserS (cur)
    a5<- parserINT (cur & value .~ a1^.value)                    
    return (cur & value .~ Expr (a5^.value) & vars %~ insert (a1^.value) )
  <|>
  do 
    a1<- parserE (cur)                    
    return (cur & value .~ Expr (a1^.value) )
  <|>
  do 
    a1<- parserPRINT (cur)                    
    return (cur & value .~ Expr (a1^.value) )                          
                          
parserINT cur =                     
  do 
    a1<- token13 (cur)
    a2<- parserS (cur)
    a3<- token6 (cur)
    a4<- parserS (cur)
    a5<- token18 (cur)
    a6<- parserS (cur)
    a7<- token6 (cur)
    a8<- parserS (cur)
    a9<- token7 (cur)
    a10<- parserS (cur)
    a11<- token7 (cur)
    a12<- parserS (cur)                    
    return (cur & value %~ Input )                          
                          
parserPRINT cur =                     
  do 
    a1<- token17 (cur)
    a2<- parserBR (cur)                    
    return (cur & value .~ Print (a2^.value) )                          
                          
parserTAB cur =                     
  do 
    a1<- token20 (cur)
    a2<- parserTAB (cur & pos %~ (\x -> x - 1) )                    
    return (cur)
  <|>
  do 
    a1<- parserS (cur)
    unless (a1^.pos == 0) nothing                     
    return (cur)                          
                          
parserS cur =                     
  do 
    a1<- token12 (cur)                    
    return (cur)
  <|>
  do                     
    return (cur)                          
                                                              
                                                                
-- | Generated parser                                           
parser x = (runParser ((^. value) <$> (parserPROGRAM x))) . lexer                 
                                                                
-- | After block                                                
instance Show DataTree where
    show (BinOp op l r) = join (" " ++ op ++ " ") [show l, show r]
    show (Const val) = val
    show (Bracket x) = " (" ++ (show x) ++ ") "
    show (While x y) =
        "while (" ++ (show x) ++ ") {\n\t"
        ++ (replace "\n" (const "\n\t") (show y))
        ++ "\n\125"
    show (If x y z) =
        "if (" ++ (show x) ++ ") {\n\t"
        ++ (replace "\n" (const "\n\t") (show y))
        ++ "\n\125" ++
        (   if (not . null . show $ z)
            then (
            " else {\n\t"
            ++ (replace "\n" (const "\n\t") (show z))
            ++ "\n\125"
            )
            else "")
    show (BlockTail x y) = join "\n" [show x, show y]
    show (Block x var) =
        (if not . null $ var
        then "int " ++ (genericJoin ", " (toList var)) ++ ";\n"
        else "")
        ++ show x
    show (Input x) = "scanf(\"%d\", &" ++ (show x) ++ ")"
    show (Print x) = "printf(\"%d\"," ++ (show x) ++ ")"
    show (Main x) =
        "#include <stdio.h>         \n\
        \int main() {               \n\t"
        ++ (replace "\n" (const "\n\t") (show x))
        ++ "return 0; \n \125"
    show (Expr x) = (show x) ++ ";"
    show Nop = []

inside :: Token -> DataTree
inside (TokenNum x) = Const x
inside (TokenName x) = Const x

interpret :: String -> Maybe String
interpret str = show <$> (fst <$> (parser (SheolAttributes 0 Nop empty) str))

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
                                                              
