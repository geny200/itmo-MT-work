module Generator.Lexer.Lexer
  ( -- * Lexer
    lexer,
  )
where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Lens.Operators ((%~), (.~))
import Generator.Lexer.Template (TMPCommonLexer (..), TMPParserToken (..), doAfter, doBefore, expression, lexerName, readToken, tmpParsers, tokenName)
import Generator.Parser.Combinator (element, greedily, parseChangFgBr, parseFigureBlock, satisfy, space, spaceStr, stream)
import Generator.Parser.Parser (Parser (..))

lexer :: Parser Char TMPCommonLexer
lexer =
  ( (.)
      <$> ( (.) <$> parseFigureBlock doBefore
              <*> greedily parseKeyWord
          )
      <*> parseFigureBlock doAfter
  )
    <*> pure (TMPCommonLexer [] "lexer" "a" [] []) <* space

parseKeyWord :: Parser Char (TMPCommonLexer -> TMPCommonLexer)
parseKeyWord =
  space >> element '%'
    >> ( (stream "lexername" >> parseChangFgBr lexerName)
           <|> (stream "tokentype" >> parseChangFgBr tokenName)
           <|> (stream "token" >> greedily parseTokens)
       )

parseTokens :: Parser Char (TMPCommonLexer -> TMPCommonLexer)
parseTokens =
  (\token -> tmpParsers %~ (token :))
    <$> ( (>>>)
            <$> parseExpression
              <*> (parseChangFgBr readToken <|> pure id)
              <*> pure (TMPParserToken [] "read")
        )

parseExpression :: Parser Char (TMPParserToken -> TMPParserToken)
parseExpression = (expression .~) <$> (space >> exprSkipLastWP)

exprSkipLastWP :: Parser Char String
exprSkipLastWP =
  (++) <$> ((++) <$> spaceStr <*> ((: []) <$> satisfy (\x -> x /= '{' && x /= '\n')))
    <*> (exprSkipLastWP <|> pure "")
