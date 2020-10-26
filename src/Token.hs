module Token
  ( 
    Token (..)
  , lexicalAnalyzer
  )
where

import Control.Applicative
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Data.Char                      (isAlpha, isSpace)
import Data.Maybe                     (fromMaybe)

data Token
  = Var
  | And
  | Or
  | Xor
  | Not
  | BrOpen
  | BrClose
  deriving (Show, Eq)

checkHead :: (Alternative f) => (Char -> Bool) -> String -> f ()
checkHead _ [] = empty
checkHead f (x : _)
  | f x = pure ()
  | otherwise = empty

char :: Char -> StateT String Maybe [Token]
char c =
  do
    s <- get
    checkHead (c ==) s
    put . tail $ s
    return empty

string :: String -> StateT String Maybe [Token]
string [] = empty
string [c] = char c
string (c : res) =
  do
    _ <- char c
    string res

binOp :: Token -> String -> StateT String Maybe Token
binOp token str =
  do
    _ <- string str
    return token

testVar :: StateT String Maybe Token
testVar =
  do
    s <- get
    checkHead isAlpha s
    put . tail $ s
    return Var

testAll :: StateT String Maybe Token
testAll 
  =     binOp And "and"
    <|> binOp Not "not"
    <|> binOp Or "or"
    <|> binOp Xor "xor"
    <|> binOp BrOpen "("
    <|> binOp BrClose ")"
    <|> testVar

lexicalAnalyzer :: String -> [Token]
lexicalAnalyzer str = fst (fromMaybe ([], []) (runStateT _lexicalAnalyzer (filter (not . isSpace) str)))

_lexicalAnalyzer :: StateT String Maybe [Token]
_lexicalAnalyzer =
  do
    b <- testAll
    e <- _lexicalAnalyzer
    return (b : e)
    <|> return []
