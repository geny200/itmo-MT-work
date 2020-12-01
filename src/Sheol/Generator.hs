{-# LANGUAGE FlexibleContexts #-}

module Sheol.Generator
  ( -- * Parser - generator
    parseTmp,
  )
where

import Control.Applicative ((<**>), (<|>))
import Control.Lens (ix, (%~), (.~))
import Parser.Combinator (allWhile, array, element, greedily, parseFigureBr, regExp, skipFigureBr, space, stream, word, wordSp)
import Parser.Parser (Parser (..))
import Sheol.Template --(TMPParser (..), TMPParserOption(..), TMPContext(..), TMPAttribute(..), TMPCommonParser(..), TMPDataAttribute(..))

defaultTMPCommonParser :: TMPCommonParser
defaultTMPCommonParser = TMPCommonParser [] (TMPDataAttribute [] []) [] [] [] [] []

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
    <*> pure defaultTMPCommonParser

parseFigureBlock field = ((field .~) <$> parseFigureBr) <|> pure id

parseKeyWord :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseKeyWord =
  element '%'
    >> ( (stream "attribute" >> parseAttribute)
           <|> (stream "attributetype" >> parseChangFgBr (attribute . definition))
           <|> (stream "tokentype" >> parseChangFgBr tokenName)
           <|> (stream "name" >> parseChangWord parserName)
           <|> (stream "lexername" >> parseChangWord lexerName)
           <|> (element '%' >> parseGramma)
       )

parseChangWord field = (field .~) <$> (space >> word)

parseChangFgBr field = (field .~) <$> parseFigureBr

parseAttribute :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseAttribute =
  (\atr -> attribute . attributes %~ (atr :))
    <$> (TMPAttribute <$> (space >> word) <*> parseFigureBr)

parseGramma :: Parser Char (TMPCommonParser -> TMPCommonParser)
parseGramma = greedily ((\pars -> tmpParsers %~ (pars :)) <$> parseGrammaParser)

parseGrammaParser :: Parser Char TMPParser
parseGrammaParser =
  (defaultTMPParser <$> (space >> word))
    <**> (dblColon >> parseChangFgBr returnType <|> pure id)
    <**> parseGrammaOptions

parseGrammaOptions :: Parser Char (TMPParser -> TMPParser)
parseGrammaOptions = greedily ((\opt -> options %~ (opt :)) <$> (parseGrammaOption <*> pure defaultTMPParserOption))

parseGrammaOption :: Parser Char (TMPParserOption -> TMPParserOption)
parseGrammaOption = (.) <$> parseGrammaOptionVar <*> (skipFigureBr (greedily parseGrammaOptionContex) <|> pure id)

-- { $2 :: attrName .~ = ...; attrName = ...;
--   $3 :: ....;
--   $$ :: ....;
--   $$ = .....;
--   where condition
--  }
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
           <|> ( stream "$$"
                   >> dblColon
                   >> (expression .~)
                   <$> array (codeLine <* semicolon)
               )
           <|> ( stream "where"
                   >> (condition .~)
                   <$> codeLine <* semicolon
               )
       )

parseGrammaOptionVar :: Parser Char (TMPParserOption -> TMPParserOption)
parseGrammaOptionVar = greedily ((\var -> variables %~ (TMPContext var [] :)) <$> (space >> wordSp))

dblColon :: Parser Char ()
dblColon = space >> stream "::" >> space

semicolon :: Parser Char ()
semicolon = space >> element ';' >> space

codeLine :: Parser Char String
codeLine = space >> allWhile (\x -> x /= ';' && x /= '}')
