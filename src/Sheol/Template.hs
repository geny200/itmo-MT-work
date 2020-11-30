{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Sheol.Template where

import Control.Applicative ((<|>))
import Control.Lens
import Parser.Parser
import Sheol.Utils (genericJoin, join, replace)
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

data TMPContext = TMPContext
  { _variable :: String,
    _context :: [String]
  }

data TMPParserOption = TMPParserOption
  { _variables :: [TMPContext],
    _expression :: String,
    _condition :: String
  }

data TMPParser = TMPParser
  { _name :: String,
    _tokenType :: String,
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

makeLenses ''TMPContext
makeLenses ''TMPAttribute
makeLenses ''TMPDataAttribute
makeLenses ''TMPParser
makeLenses ''TMPParserOption
makeLenses ''TMPCommonParser

instance Show TMPContext where
  show :: TMPContext -> String
  show contx =
    printf
      "<- %s (cur%s)"
      (toTmpName (contx ^. variable))
      (replaceChild (genericJoin " & " (contx ^. context)))

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

replaceChild :: String -> String
replaceChild = replace "\\$[0-9]+" (set _head 'a')

instance Show TMPParserOption where
  show :: TMPParserOption -> String
  show opt =
    printf
      "do                        \n\
      \  %s                      \n\
      \  %s                      \n\
      \  return (%s)"
      ( join
          "\n  "
          ( zip [1 :: Int ..] (opt ^. variables)
              <&> _1 %~ (('a' :) . show)
              <&> _2 %~ show
              <&> (^. each)
          )
      )
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

instance Show TMPParser where
  show :: TMPParser -> String
  show token =
    printf
      "%s                          \n\
      \%s cur =                    \n\
      \%s                          \n"
      ( createDefinition
          (toTmpName (token ^. name))
          (token ^. tokenType)
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
      \errorEndPoint :: Parser Token a                                 \n\
      \errorEndPoint = Parser sheolError                               \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parser                                           \n\
      \%s                                                              \n\
      \%s = %s . %s                                                    \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (parser ^. doBefore)
      ((parser ^. tmpParsers & each . tokenType .~ (parser ^. tokenName) & each %~ show) ^. each)
      ( createDefinition
          (parser ^. parserName)
          (parser ^. tokenName)
          (parser ^. tmpParsers ^? _head ^?! _Just ^. returnType)
      )
      (parser ^. parserName)
      (parser ^. parserName)
      (parser ^. lexerName)
      (parser ^. doAfter)

type Token = Char

action1 :: TMPAttribute -> TMPAttribute
action1 x = x & attributeName .~ "new name" & attributeType .~ "new type"

-- { $2 :: attrName .~ = ...; attrName = ...;
--   $3 :: ....;
--   $$ :: ....;
--   $$ = .....;
--   where condition
--  }
test1 :: TMPAttribute -> Parser Token TMPAttribute
test1 cur =
  do
    a1 <- test1 (action1 cur)
    --condition
    return a1
    <|> do
      a1 <- test1 (action1 cur)
      --condition
      return a1
    <|> errorEndPoint

errorEndPoint :: Parser Token a
errorEndPoint = Parser sheolError

sheolError :: [Token] -> a
sheolError _ = error ("Parse error\n")
