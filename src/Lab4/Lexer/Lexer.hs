module Lab4.Lexer.Lexer
  ( -- * Lexer
    lexer
  )
where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Lens.Operators ((%~), (.~))
import Lab4.Lexer.Template
  ( TMPCommonLexer (..), TMPParserToken (..),
    doAfter, doBefore, expression, lexerName,
    readToken, tmpParsers, tokenName
  )
import Lab4.Parser.Combinator 
  ( element, greedily, parseChangFgBr, parseFigureBlock, 
  satisfy, space, spaceStr, stream
  )
import Lab4.Parser.Parser (Parser (..))

-- | A parser that generates a lexer for a given grammar
lexer :: Parser Char TMPCommonLexer
lexer =
  ( (.)
      <$> ( (.) <$> parseFigureBlock doBefore
              <*> greedily parseKeyWord
          )
      <*> parseFigureBlock doAfter
  )
    <*> pure (TMPCommonLexer [] "lexer" "a" [] []) <* space

-- | Parser for parsing service words
parseKeyWord :: Parser Char (TMPCommonLexer -> TMPCommonLexer)
parseKeyWord =
  space >> element '%'
    >> ( (stream "lexername" >> parseChangFgBr lexerName)
           <|> (stream "tokentype" >> parseChangFgBr tokenName)
           <|> (stream "token" >> greedily parseTokens)
       )

-- | Parser for parsing token data
parseTokens :: Parser Char (TMPCommonLexer -> TMPCommonLexer)
parseTokens =
  (\token -> tmpParsers %~ (token :))
    <$> ( (>>>)
            <$> parseExpression
              <*> (parseChangFgBr readToken <|> pure id)
              <*> pure (TMPParserToken [] "read")
        )

-- | Parser for reading expressions (regular expressions)
parseExpression :: Parser Char (TMPParserToken -> TMPParserToken)
parseExpression = (expression .~) <$> (space >> exprSkipLastWP)

-- | Parser for reading data without the last spaces
exprSkipLastWP :: Parser Char String
exprSkipLastWP =
  (++) <$> ((++) <$> spaceStr <*> ((: []) <$> satisfy (\x -> x /= '{' && x /= '\n')))
    <*> (exprSkipLastWP <|> pure "")
