module Lab2.Token
  ( -- * Token constructors
    Token (..),

    -- * Function
    lexicalAnalyzer,
  )
where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Data.Char (isAlpha, isSpace)

-- | Date type to represent all possible supported
-- tokens for the parser.
data Token
  = Var                 -- ^ token variable
  | And                 -- ^ token is a binary operation `and`
  | Or                  -- ^ token is a binary operation `or`
  | Xor                 -- ^ token is a binary operation `xor`
  | Not                 -- ^ token is a binary operation `not`
  | BrOpen              -- ^ token is opening bracket `(`
  | BrClose             -- ^ token is closing bracket `)`
  deriving (Show, Eq)

-- | Converts a @String@ to a @List@ of parsed tokens
-- (or an error message, if the token is not recognized).
lexicalAnalyzer :: String -> Either String [Token]
lexicalAnalyzer str = resultAnalyzer (runStateT _lexicalAnalyzer (filter (not . isSpace) str))

-- | Generates an error message if token parsing failed
resultAnalyzer :: Maybe ([Token], String) -> Either String [Token]
resultAnalyzer (Just (tokens, [])) = Right tokens
resultAnalyzer (Just (_, x : _)) = Left ("unexpected char " ++ [x])
resultAnalyzer Nothing = Left "lexical analyzer error"

-- | Recursively parses the tokens
_lexicalAnalyzer :: StateT String Maybe [Token]
_lexicalAnalyzer =
  do
    b <- testAll
    e <- _lexicalAnalyzer
    return (b : e)
    <|> return []

---------------------------------------------------
---        Functions for parsing tokens         ---
---------------------------------------------------

-- | Checks for the entry of any token.
testAll :: StateT String Maybe Token
testAll =
  binOp And "and"
    <|> binOp Not "not"
    <|> binOp Or "or"
    <|> binOp Xor "xor"
    <|> binOp BrOpen "("
    <|> binOp BrClose ")"
    <|> testVar

-- | Checks the first character of the string by predicate
checkHead :: (Alternative f) => (Char -> Bool) -> String -> f ()
checkHead _ [] = empty
checkHead f (x : _)
  | f x = pure ()
  | otherwise = empty

-- | Checks for the entry of given @Char@.
char :: Char -> StateT String Maybe [Token]
char c =
  do
    s <- get
    checkHead (c ==) s
    put . tail $ s
    return empty

-- | Checks for the entry of given @String@.
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

-- | Checks for the entry of a variable.
testVar :: StateT String Maybe Token
testVar =
  do
    s <- get
    checkHead isAlpha s
    put . tail $ s
    return Var
