{-# LANGUAGE FlexibleContexts #-}

module Lab4.Sheol.Generate
  ( -- * Parser - generator
    parseTmp,
  )
where

import Control.Applicative ((<**>), (<|>))
import Control.Category ((>>>))
import Control.Lens (ix, (%~), (.~))
import Lab4.Parser.Combinator (allWhile, array, element, greedily, parseChangFgBr, parseChangWord, parseFigureBlock, parseFigureBr, regExp, skipFigureBr, space, stream, word, wordSp)
import Lab4.Parser.Parser (Parser (..))
import Lab4.Sheol.Template

defaultTMPCommonParser :: TMPCommonParser
defaultTMPCommonParser = TMPCommonParser [] (TMPDataAttribute [] []) [] [] [] [] [] []

defaultTMPParser :: String -> TMPParser
defaultTMPParser strName = TMPParser strName [] [] []

defaultTMPParserOption :: TMPParserOption
defaultTMPParserOption = TMPParserOption [] [] []

parseTmp :: Parser Char TMPCommonParser
parseTmp =
  ( (.)
      <$> ( (.) <$> parseFigureBlock doBefore
              <*> greedily parseKeyWord -- ((.) <$> parseKeyWord <*> parseAll) <|> pure id
          )
      <*> parseFigureBlock doAfter
  )
    <*> pure defaultTMPCommonParser <* space

parseKeyWord :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseKeyWord =
  space >> element '%'
    >> ( (stream "attributetype" >> parseChangFgBr (attribute . definition))
           <|> (stream "attribute" >> parseAttribute)
           <|> (stream "tokentype" >> parseChangFgBr tokenName)
           <|> (stream "token" >> greedily (space >> parseToken <* space))
           <|> (stream "name" >> parseChangWord parserName)
           <|> (stream "lexername" >> parseChangWord lexerName)
           <|> (element '%' >> parseGramma)
       )

parseToken :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseToken =
  (\token -> tokens %~ (token :))
    <$> ((.) <$> ((pattern .~) <$> (space >> wordSp)) <*> parseTokenExpr <*> pure (TMPToken [] [] []))

parseTokenExpr :: Parser Char (TMPToken -> TMPToken)
parseTokenExpr = skipFigureBr ((.) <$> ((tokenExpr .~) <$> codeLine) <*> (semicolon >> parseReturnExpression (returnExpr .~) <|> pure id))

parseAttribute :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseAttribute =
  (\atr -> attribute . attributes %~ (atr :))
    <$> (TMPAttribute <$> (space >> word) <*> parseFigureBr)

parseGramma :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseGramma = greedily ((\pars -> tmpParsers %~ (pars :)) <$> parseGrammaParser)

parseGrammaParser :: Parser Char TMPParser
parseGrammaParser =
  (defaultTMPParser <$> (space >> word))
    <**> ((dblColon >> parseChangFgBr returnType) <|> pure id)
    <**> (space >> element ':' >> parseGrammaOptions)

parseGrammaOptions :: Parser Char (TMPParser -> TMPParser)
parseGrammaOptions =
  (.) <$> ((\opt -> options %~ (opt :)) <$> (parseGrammaOption <*> pure defaultTMPParserOption))
    <*> greedily ((\opt -> options %~ (opt :)) <$> (space >> element '|' >> parseGrammaOption <*> pure defaultTMPParserOption))

parseGrammaOption :: Parser Char (TMPParserOption -> TMPParserOption)
parseGrammaOption = (>>>) <$> (parseGrammaOptionVar <|> pure id) <*> (skipFigureBr (greedily parseGrammaOptionContex) <|> pure id)

parseGrammaOptionContex :: Parser Char (TMPParserOption -> TMPParserOption)
parseGrammaOptionContex =
  space
    >> ( ( do
             num <- regExp "^\\$[0-9]+"
             let nuh = read (tail num) - 1
             dblColon
             (variables . ix (nuh :: Int) . context .~)
               <$> array (codeLine <* semicolon)
         )
           <|> parseReturnExpression (expression .~)
           <|> ( stream "where"
                   >> (condition .~)
                   <$> codeLine <* semicolon
               )
       )
      <* semicolon

parseReturnExpression field =
  ( stream "$$"
      >> dblColon
      >> field
      <$> array (codeLine <* semicolon)
  )

parseGrammaOptionVar :: Parser Char (TMPParserOption -> TMPParserOption)
parseGrammaOptionVar = greedily ((\var -> variables %~ (TMPContext var [] :)) <$> (space >> wordSp))

dblColon :: Parser Char ()
dblColon = space >> stream "::" >> space

semicolon :: Parser Char ()
semicolon = (space >> element ';' >> space) <|> pure ()

codeLine :: Parser Char String
codeLine = space >> allWhile (\x -> x /= ';' && x /= '}')
