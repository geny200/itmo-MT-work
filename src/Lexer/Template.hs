{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lexer.Template where

import Control.Lens
import Text.Printf (printf)

toTmpName :: Integer -> String
toTmpName x = "tokenParse" ++ show x

data TMPParserToken = TMPParserToken
  { _number :: Integer,
    _expression :: String,
    _readToken :: String
  }

data TMPCommonLexer = TMPCommonLexer
  { _doBefore :: String,
    _lexerName :: String,
    _tokenName :: String,
    _tmpParsers :: [TMPParserToken],
    _doAfter :: String
  }

makeLenses ''TMPParserToken
makeLenses ''TMPCommonLexer

toTokenParser :: String -> TMPParserToken -> String
toTokenParser typeName token =
    printf
      "%s :: Parser Char %s\n\
      \%s = (%s) <$> regExp \"^%s\"\n\n"
      (toTmpName (token ^. number))
      typeName
      (toTmpName (token ^. number))
      (token ^. readToken)
      (token ^. expression)

--name :: (Read a) => Parser Char a
--name = read <$> regExp nameRegexp

tmpToNames :: [TMPParserToken] -> String
tmpToNames = foldl1 (\x y -> x ++ " <|> " ++ y) . foldr (\x -> (toTmpName (x ^. number) :)) []

instance Show TMPCommonLexer where
  show :: TMPCommonLexer -> String
  show lexer =
    printf
      "\
      \module Lexer                                                    \n\
      \  ( -- * lexer parser                                           \n\
      \    %s                                                          \n\
      \  )                                                             \n\ 
      \where                                                                \n\
      \                                                                \n\
      \import Control.Applicative ((<|>))                              \n\
      \import Parser.Combinator (eof, regExp)                          \n\
      \import Parser.Parser (Parser (..))                              \n\
      \                                                                \n\
      \-- | Before block                                               \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Token parsers                                              \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Union of all token parsers                                 \n\
      \commonLexer :: Parser Char %s                                    \n\
      \commonLexer = %s                                                \n\
      \                                                                \n\
      \-- | Generated lexer                                            \n\
      \%s :: Parser Char [%s]                                          \n\
      \%s = ((:) <$> commonLexer <*> %s) <|> (eof >> pure [])          \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (lexer ^. lexerName)
      (lexer ^. doBefore)
      (concatMap (toTokenParser (lexer ^. tokenName)) (lexer ^. tmpParsers))
      (lexer ^. tokenName)
      (tmpToNames (lexer ^. tmpParsers))
      (lexer ^. lexerName)
      (lexer ^. tokenName)
      (lexer ^. lexerName)
      (lexer ^. lexerName)
      (lexer ^. doAfter)

--commonLexer :: Parser Char T
--commonLexer = name <|> name ...
--
--nameLexer :: Parser Char [T]
--nameLexer =
--  ((:) <$> commonLexer <*> nameLexer)
--    <|> (eof >> pure [])
