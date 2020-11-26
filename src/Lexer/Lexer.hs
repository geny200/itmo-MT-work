module Lexer.Lexer where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (State, get, evalState, modify)
import Data.Char (isSpace)
import Lexer.Template (TMPCommonLexer (..), TMPParserToken (..))
import Parser.Combinator
import Parser.Parser (Parser (..))

lexer :: Parser Char TMPCommonLexer
lexer = undefined

parseTmpLexer :: Parser Char TMPCommonLexer
parseTmpLexer =
  TMPCommonLexer
    <$> (parseFigureBr <|> pure "")
    <*> ((space >> stream "%%lexername" >> parseFigureBr) <|> pure "lexer")
    <*> ((space >> stream "%%tokentype" >> parseFigureBr) <|> pure "a")
    <*> (space >> stream "%%tokens" >> evalState parseTmpTokens 0)
    <*> (parseFigureBr <|> pure "")
    
parseTmpTokens :: State Integer (Parser Char [TMPParserToken])
parseTmpTokens =
  do
    x <- parseTmpToken
    xs <- parseTmpTokens
    return ((:) <$> x <*> (xs <|> pure []))

parseTmpToken :: State Integer (Parser Char TMPParserToken)
parseTmpToken =
  do
    modify (+ 1)
    uniqNum <- get
    return
      ( TMPParserToken uniqNum
          <$> expression
          <*> (parseFigureBr <|> pure "read")
      )

expression :: Parser Char String
expression = space >> exprSkipLastWP

exprSkipLastWP :: Parser Char String
exprSkipLastWP =
  (++) <$> ((++) <$> spaceStr <*> ((: []) <$> satisfy (\x -> x /= '{' && x /= '\n')))
    <*> (exprSkipLastWP <|> pure "")

skipLastWP :: Parser Char a -> Parser Char a 
skipLastWP = undefined

skipFigureBr :: Parser Char a -> Parser Char a
skipFigureBr pars = space >> satisfy (== '{') >> space >> pars >>= (\x -> space >> satisfy (== '}') >> return x)

parseFigureBr :: Parser Char String
parseFigureBr = skipFigureBr (allWhile (/= '}'))

-- | A parser that consumes any number
-- of whitespace characters.
spaceStr :: Parser Char String
spaceStr = ((:) <$> satisfy isSpace) <*> spaceStr <|> pure ""
