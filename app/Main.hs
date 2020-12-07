module Main where

--import Graph (evaluateGraph)
import Generator.Parser.Parser (Parser(..))
import System.IO (IOMode(..), openFile, hGetContents, hClose)
import Data.Maybe (fromJust)
import Interpreter.Calculator.Parser (calculate)
import Utils (join)
import Generator.Sheol.Generate (parseTmp)
import Generator.Lexer.Lexer (lexer)
import Interpreter.PythonToC.Parser (interpret)
import Interpreter.PythonToC.Lexer (lexer)

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
      
runFromFile ::(Show a) => String -> String -> (String -> Maybe a) -> IO ()
runFromFile fromFile toFile action = 
    do 
      handle <- openFile fromFile ReadMode
      everything <- hGetContents handle
      writeFile toFile (show . fromJust . action $ everything)
      hClose handle
      
generic :: String -> String -> String -> IO ()
generic name from to =
  do 
    runFile (join "/" [from, name ++ ".ly"]) (join "/" [to, "Parser.hs"]) parseTmp
    runFile (join "/" [from, name ++ ".lex"]) (join "/" [to, "Lexer.hs"]) Generator.Lexer.Lexer.lexer 

main :: IO ()
main =
  do 
    --generic "PyToC" "resources/PythonToC" "src/Interpreter/PythonToC/"
    --generic "Calc" "resources/Calculator" "src/Interpreter/Calculator/"
    runFromFile "test_calc.in" "test_calc.out" calculate
    runFromFile "test_py.in" "test_py.out" interpret
    runFromFile "test_py.in" "test_py.lex" (Just . Interpreter.PythonToC.Lexer.lexer)
    runFromFile "test_py.in" "test_py.check" Just


--print (interpret "2+2*2;")
--generic "PyToC" "resources/PythonToC" "src/Interpreter/PythonToC/"
--generic "Calc" "resources/Calculator" "src/Interpreter/Calculator/"
--print (calculate "2+2*2")

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