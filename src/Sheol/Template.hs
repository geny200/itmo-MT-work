{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Sheol.Template where

import Control.Applicative ((<|>))
import Control.Lens
import Parser.Parser
import Sheol.Utils (genericJoin, replace)
import Text.Printf (printf)

toTmpName :: String -> String
toTmpName x = "parser" ++ x

data TMPAttribute = TMPAttribute
  { _attributeName :: String,
    _attributeType :: String
  }

data TMPDataAttribute = TMPDataAttribute
  { _definition :: String,
    _attributes :: [TMPAttribute]
  }

data TMPParserOption = TMPParserOption
  { _variables :: [String],
    _expression :: String,
    _condition :: String
  }

data TMPParser = TMPParser
  { _name :: String,
    _returnType :: String,
    _options :: [TMPParserOption]
  }

data TMPCommonParser = TMPCommonParser
  { _doBefore :: String,
    _attribute :: TMPDataAttribute,
    _lexerName :: String,
    _parserName :: String,
    _tokenName :: String,
    _tmpParsers :: [TMPParser],
    _doAfter :: String
  }

makeLenses ''TMPAttribute
makeLenses ''TMPDataAttribute
makeLenses ''TMPParser
makeLenses ''TMPParserOption
makeLenses ''TMPCommonParser

instance Show TMPAttribute where
  show :: TMPAttribute -> String
  show attribut =
    (attribut ^. attributeName)
      ++ " :: "
      ++ (attribut ^. attributeType)

instance Show TMPDataAttribute where
  show :: TMPDataAttribute -> String
  show atr =
    printf
      "data %s = SheolAttributes \n\
      \  { %s                    \n\
      \  }"
      (atr ^. definition)
      (genericJoin "\n  , " (atr ^. attributes))

varsToString :: [String] -> String
varsToString = genericJoin "\n  "

replaceChild :: String -> String
replaceChild = replace "\\$[0-9]+" (('a' :) . tail)

instance Show TMPParserOption where
  show :: TMPParserOption -> String
  show opt =
    printf
      "do                        \n\
      \%s                        \n\
      \  %s                      \n\
      \  return (%s)"
      --(opt ^. variables)
      (replaceChild (opt ^. condition))
      (replaceChild (opt ^. expression))

createDefinition :: String -> String -> String -> String
createDefinition _ _ [] = []
createDefinition defName tokType retType =
  printf
    "%s :: Parser [%s] %s"
    defName
    tokType
    retType

toTokenParser :: String -> TMPParser -> String
toTokenParser typeName token =
  printf
    "%s                          \n\
    \%s =                        \n\
    \%s                          \n"
    ( createDefinition
        (toTmpName (token ^. name))
        typeName
        (token ^. returnType)
    )
    (toTmpName (token ^. name))
    (genericJoin "\n  <|>\n" (token ^. options))

instance Show TMPCommonParser where
  show :: TMPCommonParser -> String
  show parser =
    printf
      "\
      \-- | Before block                                               \n\
      \%s                                                              \n\
      \                                                                \n\
      \import Control.Applicative ((<|>))                              \n\
      \import Parser.Parser (Parser (..))                              \n\
      \                                                                \n\
      \-- | Generated parsers                                          \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parser                                           \n\
      \%s                                                              \n\
      \%s = %s . %s                                                    \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (parser ^. doBefore)
      (concatMap (toTokenParser (parser ^. tokenName)) (parser ^. tmpParsers))
      ( createDefinition
          (parser ^. parserName)
          (parser ^. tokenName)
          (head (parser ^. tmpParsers) ^. returnType)
      )
      (parser ^. parserName)
      (parser ^. parserName)
      (parser ^. lexerName)
      (parser ^. doAfter)

type Token = Char

name' :: Parser Token a
name' =
  do
    a1 <- name'
    a2 <- name'
    return (a1)
    <|> do
      a1 <- name'
      a2 <- name'
      error "oops"
      if 1 == 0
        then pure ()
        else error "length not equal to 0"
      pure ()
      return (a1)
