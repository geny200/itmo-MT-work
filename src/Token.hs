{-# LANGUAGE BlockArguments #-}

module Token
  (
    Token (..)
  , lexicalAnalyzer
  )
where

import Control.Applicative
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Data.Char (isAlpha, isSpace)
import Data.Maybe (fromMaybe)

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

testAnd :: StateT String Maybe Token
testAnd = binOp And "and"

testOr :: StateT String Maybe Token
testOr = binOp Or "or"

testXor :: StateT String Maybe Token
testXor = binOp Xor "xor"

testNot :: StateT String Maybe Token
testNot = binOp Not "not"

testBrOpen :: StateT String Maybe Token
testBrOpen = binOp BrOpen "("

testBrClose :: StateT String Maybe Token
testBrClose = binOp BrClose ")"

testVar :: StateT String Maybe Token
testVar =
  do
    s <- get
    checkHead isAlpha s
    put . tail $ s
    return Var

testAll :: StateT String Maybe Token
testAll =
  do
    testAnd
    <|> do
      testNot
    <|> do
      testOr
    <|> do
      testXor
    <|> do
      testBrOpen
    <|> do
      testBrClose
    <|> do
      testVar

lexicalAnalyzer :: String -> [Token]
lexicalAnalyzer str = fst (fromMaybe ([], []) (runStateT _lexicalAnalyzer (filter (not . isSpace) str)))

_lexicalAnalyzer :: StateT String Maybe [Token]
_lexicalAnalyzer =
  do
    b <- testAll
    e <- _lexicalAnalyzer
    return (b : e)
    <|> do
      return []
