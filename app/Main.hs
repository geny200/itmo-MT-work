module Main where

--import Graph (evaluateGraph)
import Text.Regex.TDFA
import Text.Printf (printf)
import Lexer.Lexer (lexer)
import Parser.Parser (runParser)
import Data.Maybe (fromJust)
import System.IO
import Test

main :: IO ()
main = 
  do 
    handle <- openFile "test.lex" ReadMode
    everything <- hGetContents handle
    writeFile "src/Test.hs" (show . fst . fromJust $ runParser lexer everything)
    --print (fst . fromJust $ (runParser (parseTmpLexer) everything))
    hClose handle
--print (fst . fromJust $ (runParser (evalState parseTmpToken (TMPLexer "" 0)) "  reg exp       {           }"))
 --printf "hello %s %s" "Eugene"
--let emailRegex = "^[0-9]+" in print ("125 martin" =~~ emailRegex :: Maybe (String, String, String))
--  do 
--    x <- getLine
--    pure (error "oops")
--    print x

--let emailRegex = "^[0-9]+" in print ("hello 125 martin" =~~ emailRegex :: Maybe (String, String, String))
---evaluateGraph "(c and (not (not f)) or x) or (t xor f and (r))  "