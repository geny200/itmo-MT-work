module Main where

--import Graph (evaluateGraph)
import Text.Regex.TDFA
import Text.Printf (printf)
import Lexer.Lexer (lexer)
import Parser.Parser (runParser)
import Data.Maybe (fromJust)
import System.IO
import Test
import Sheol.Generator
import Parser.Combinator
import Sheol.Template
import Parser.Parser
import Test.Parser
import Test.Lexer

testCodeLine :: String
testCodeLine = "SND TRH { $1 :: pos .~ 0; value .~ 0;; $$ :: pos .~ ($2 ^. pos);; $2 :: pos .~ ($1 ^~ pos);;  }"
myTest :: String
myTest =
  "%name pareser                  \n\
  \%lexername lexer               \n\
  \%attributetype { MyData a }    \n\
  \%attribute pos { Int }         \n\
  \%attribute value { String }    \n\
  \%%                             \n\
  \FST : | '+' '-' { $1 :: pos .~ 0; value .~ 0;; $$ :: pos .~ ($2 ^. pos);; $2 :: pos .~ ($1 ^~ pos);;  }\n\
  \ | '+'                         \n\
  \%token '+' { Plus }            \n\
  \       '-' { Minus }           \n\
  \                               \n\
  \                               \n\
  \                               \n\
  \                               \n\
  \                               \n\
  \                               \n\
  \                               \n\
  \"

runFile :: (Show a) => String -> String -> Parser Char a -> IO ()
runFile fromFile toFile pars =
    do 
      handle <- openFile fromFile ReadMode
      everything <- hGetContents handle
      writeFile toFile (show . fst . fromJust $ runParser pars everything)
      hClose handle 

main :: IO ()
main = print (calculate "2+2*2")

--  do 
--    runFile "test.ly" "src/Test/Parser.hs" parseTmp
--    runFile "test.lex" "src/Test/Lexer.hs" Lexer.Lexer.lexer



  --print (runParser parseTmp myTest)
--print (someG [TMPToken "\'+\'" [], TMPToken "let" []])
--print (runParser parseTmp myTest)
--print (runParser (skipFigureBr (greedily parseGrammaOptionContex) <*> pure (TMPParserOption [TMPContext "SND" [], TMPContext "TRH" []] [] []) ) testCodeLine)
 --print (runParser parseTmp myTest)
--print (runParser (element '%' >> stream "name" >> space >> word) "%name pareser")
--
--  do 
--    handle <- openFile "test.ly" ReadMode
--    everything <- hGetContents handle
--    writeFile "src/Test/Parser.hs" (show . fst . fromJust $ runParser parseTmp everything)
--    --print (fst . fromJust $ (runParser (parseTmpLexer) everything))
--    hClose handle
    
--print (fst . fromJust $ (runParser (evalState parseTmpToken (TMPLexer "" 0)) "  reg exp       {           }"))
 --printf "hello %s %s" "Eugene"
--let emailRegex = "^[0-9]+" in print ("125 martin" =~~ emailRegex :: Maybe (String, String, String))
--  do 
--    x <- getLine
--    pure (error "oops")
--    print x

--let emailRegex = "^[0-9]+" in print ("hello 125 martin" =~~ emailRegex :: Maybe (String, String, String))
---evaluateGraph "(c and (not (not f)) or x) or (t xor f and (r))  "