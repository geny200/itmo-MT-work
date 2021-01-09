module Lab4.Example
  ( -- * Function
    testGeneric
  )
where

import Data.Maybe (fromJust)
import Lab4.Lexer.Lexer (lexer)
import Lab4.Parser.Parser (Parser, runParser)
import Lab4.Sheol.Generate (parseTmp)
import System.IO (IOMode (..), hClose, hGetContents, openFile)
import Utils (join)

-- | To start the generation of the parser grammar.
testGeneric :: IO ()
testGeneric =
  do
    generic "PyToC" "resources/PythonToC" "src/Lab3/PythonToC/"
    generic "Calc" "resources/Calculator" "src/Lab3/Calculator/"

-- | Run the generation of the parser grammar.
generic :: String -> String -> String -> IO ()
generic name from to =
  do
    runFile (join "/" [from, name ++ ".ly"]) (join "/" [to, "Parser.hs"]) parseTmp
    runFile (join "/" [from, name ++ ".lex"]) (join "/" [to, "Lexer.hs"]) lexer

runFile :: (Show a) => String -> String -> Parser Char a -> IO ()
runFile fromFile toFile pars =
  do
    handle <- openFile fromFile ReadMode
    everything <- hGetContents handle
    writeFile toFile (show . fst . fromJust $ runParser pars everything)
    hClose handle
