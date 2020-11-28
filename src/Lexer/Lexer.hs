module Lexer.Lexer
  (
    lexer
  )
where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (State, evalState, get, modify)
import Lexer.Template (TMPCommonLexer (..), TMPParserToken (..))
import Parser.Combinator (parseFigureBr, satisfy, space, spaceStr, stream)
import Parser.Parser (Parser (..))

lexer :: Parser Char TMPCommonLexer
lexer =
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
