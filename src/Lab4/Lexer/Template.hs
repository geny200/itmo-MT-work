{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lab4.Lexer.Template
  ( 
    TMPCommonLexer (..),
    TMPParserToken (..),
    doAfter,
    doBefore,
    expression,
    lexerName,
    readToken,
    tmpParsers,
    tokenName,
  )
where

import Control.Lens (each, makeLenses, (%~), (<&>), (^.), _1, _2)
import Text.Printf (printf)
import Utils (join)

toTmpName :: Integer -> String
toTmpName x = "tokenParse" ++ show x

data TMPParserToken = TMPParserToken
  { _expression :: String,
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

instance Show TMPParserToken where
  show :: TMPParserToken -> String
  show token =
    printf
      " = (%s) <$> regExp \"^%s\"\n"
      (token ^. readToken)
      (token ^. expression)

--name :: (Read a) => Parser Char a
--name = read <$> regExp nameRegexp

instance Show TMPCommonLexer where
  show :: TMPCommonLexer -> String
  show lexer =
    printf
      "\
      \-- | Before block                                               \n\
      \%s                                                              \n\
      \                                                                \n\
      \import Lab4.Parser.Combinator (eof, regExp)                     \n\
      \import Lab4.Parser.Parser (Parser (..), (<|>))                  \n\
      \                                                                \n\
      \-- parser produced by SheolLexer Version 1.0.0                  \n\
      \                                                                \n\
      \-- | Token parsers                                              \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Union of all token parsers                                 \n\
      \commonLexer :: Parser Char %s                                   \n\
      \commonLexer = %s                                                \n\
      \                                                                \n\
      \-- | Generated lexer                                            \n\
      \%s :: Parser Char [%s]                                          \n\
      \%s = ((:) <$> commonLexer <*> %s) <|> (eof >> pure [])          \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (lexer ^. doBefore)
      ( join
          "\n"
          ( zip [1 :: Integer ..] (lexer ^. tmpParsers)
              <&> _1 %~ toTmpName
              <&> _2 %~ show
              <&> (^. each)
          )
      )
      (lexer ^. tokenName)
      ( join
          " <|> "
          ( zip [1 :: Integer ..] (lexer ^. tmpParsers)
              <&> _1 %~ toTmpName
              <&> (^. _1)
          )
      )
      (lexer ^. lexerName)
      (lexer ^. tokenName)
      (lexer ^. lexerName)
      (lexer ^. lexerName)
      (lexer ^. doAfter)
