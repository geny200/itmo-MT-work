module Parser.Combinator
  ( -- * Combinators
    array,
    arrayJoin,
    allWhile,
    element,
    eof,
    greedily,
    ok,
    parseFigureBr,
    regExp,
    satisfy,
    skipFigureBr,
    space,
    spaceStr,
    stream,
    word,
    wordSp
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Parser.Parser (Parser (..))
import Text.Regex.TDFA

-- | The parser never crashes or consumes input
ok :: Parser s ()
ok = pure ()

-- | Checks that the parser has reached the end
-- of the data stream (otherwise it fail).
eof :: Parser s ()
eof = Parser f
  where
    f [] = Just ((), [])
    f _ = Nothing

-- | The parser accepts a predicate on a stream
-- element, and returns the element, absorbing
-- it from the stream, if the predicate on the
-- element is `True`, otherwise it falls.
satisfy ::
  -- | predicate
  (s -> Bool) ->
  -- | the parser for predicate
  Parser s s
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

-- | Creates a parser that parses the specified
-- single element of the stream.
element ::
  (Eq s) =>
  -- | specified element
  s ->
  -- | the parser for that element
  Parser s s
element x = satisfy (== x)

-- | Creates a parser that parses several specified
-- stream elements.
stream ::
  (Eq s) =>
  -- | the specified elements
  [s] ->
  -- | the parser for that elements
  Parser s [s]
stream = foldr (\x -> (<*>) ((:) <$> element x)) (pure [])

-- | A parser that consumes any number
-- of whitespace characters.
space :: Parser Char ()
space =
  (satisfy isSpace >> space)
    <|> pure ()
    
allWhile :: (s -> Bool) -> Parser s [s] 
allWhile p = (:) <$> satisfy p <*> (allWhile p <|> pure [])

regExp :: String -> Parser Char String
regExp strRegExp = Parser $ \input ->
  case input =~~ strRegExp :: Maybe (String, String, String) of
    Nothing -> Nothing
    Just (_, x, xs) -> Just (x, xs)

skipFigureBr :: Parser Char a -> Parser Char a
skipFigureBr pars = space >> satisfy (== '{') >> space >> pars >>= (\x -> space >> satisfy (== '}') >> return x)

parseFigureBr :: Parser Char String
parseFigureBr = skipFigureBr (allWhile (/= '}'))

word :: Parser Char String
word = allWhile (\x -> 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z')

wordSp :: Parser Char String
wordSp = allWhile (\x -> 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z' || x == '_')

-- | A parser that consumes any number
-- of whitespace characters.
spaceStr :: Parser Char String
spaceStr = ((:) <$> satisfy isSpace) <*> spaceStr <|> pure ""

array :: Parser s a -> Parser s [a]
array pars = (:) <$> pars <*> (array pars <|> pure [])

arrayJoin :: (a -> a -> a) -> Parser s a -> a -> Parser s a
arrayJoin combine pars el = (combine <$> pars <*> arrayJoin combine pars el) <|> pure el

greedily :: Parser s (a -> a) -> Parser s (a -> a)
greedily x = arrayJoin (.) x id 
