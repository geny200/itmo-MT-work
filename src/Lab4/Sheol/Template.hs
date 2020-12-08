{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lab4.Sheol.Template where

import Control.Lens (each, makeLenses, set, (%~), (&), (.~), (<&>), (^.), (^..), (^?), (^?!), _1, _2, _Just, _head)
import Data.Char (isSpace)
import Data.List (elemIndex)
import Text.Printf (printf)
import Utils (genericJoin, join, replace)

toTmpName :: String -> String
toTmpName x = "parser" ++ x

toTmpTokenName :: Int -> String
toTmpTokenName x = "token" ++ show x

tmpCurrent :: String
tmpCurrent = "cur"

exist :: String -> String
exist [] = []
exist str = "\n    " ++ str

data TMPToken = TMPToken
  { _pattern :: String,
    _tokenExpr :: String,
    _returnExpr :: [String]
  }

data TMPAttribute = TMPAttribute
  { _attributeName :: String, --
    _attributeType :: String --
  }

data TMPDataAttribute = TMPDataAttribute
  { _definition :: String, --
    _attributes :: [TMPAttribute] --
  }

data TMPContext = TMPContext
  { _variable :: String,
    _context :: [String]
  }

data TMPParserOption = TMPParserOption
  { _variables :: [TMPContext],
    _expression :: [String],
    _condition :: String
  }

data TMPParser = TMPParser
  { _name :: String,
    _tokenType :: String,
    _returnType :: String,
    _options :: [TMPParserOption]
  }

data TMPCommonParser = TMPCommonParser
  { _doBefore :: String, --
    _attribute :: TMPDataAttribute, --
    _lexerName :: String, --
    _parserName :: String, --
    _tokenName :: String, --
    _tokens :: [TMPToken],
    _tmpParsers :: [TMPParser],
    _doAfter :: String --
  }

makeLenses ''TMPToken
makeLenses ''TMPContext
makeLenses ''TMPAttribute
makeLenses ''TMPDataAttribute
makeLenses ''TMPParser
makeLenses ''TMPParserOption
makeLenses ''TMPCommonParser

printExpression :: [String] -> String
printExpression expr = replaceChild (join " & " (tmpCurrent : expr))

instance Show TMPContext where
  show :: TMPContext -> String
  show contx =
    printf
      "<- %s (%s)"
      (contx ^. variable)
      (printExpression (contx ^. context))

instance Show TMPAttribute where
  show :: TMPAttribute -> String
  show attribut =
    "_" ++ (attribut ^. attributeName)
      ++ " :: "
      ++ (attribut ^. attributeType)

instance Show TMPDataAttribute where
  show :: TMPDataAttribute -> String
  show atr =
    printf
      "data %s = SheolAttributes \n\
      \  { %s                    \n\
      \  }                       \n\
      \                          \n\
      \makeLenses ''%s           \n"
      (atr ^. definition)
      (genericJoin "\n  , " (atr ^. attributes))
      (takeWhile (not . isSpace) (atr ^. definition))

replaceChild :: String -> String
replaceChild = replace "\\$[0-9]+" (set _head 'a')

instance Show TMPParserOption where
  show :: TMPParserOption -> String
  show opt =
    printf
      "  do %s%s                    \n\
      \    return (%s)"
      ( exist
          ( join
              "\n    "
              ( zip [1 :: Int ..] (opt ^. variables)
                  <&> _1 %~ (('a' :) . show)
                  <&> _2 %~ show
                  <&> (^. each)
              )
          )
      )
      (exist (replaceChild (opt ^. condition)))
      (printExpression (opt ^. expression))

createDefinition :: String -> String -> String -> String
createDefinition _ _ [] = []
createDefinition defName tokType retType =
  printf
    "%s :: Parser [%s] %s"
    defName
    tokType
    retType

instance Show TMPToken where
  show :: TMPToken -> String
  show token =
    printf
      " %s =                       \n\
      \  do                        \n\
      \    a1 <- satisfy func      \n\
      \    return (%s)             \n\
      \    where func (%s) = True  \n\
      \          func _ = False"
      tmpCurrent
      (printExpression (token ^. returnExpr))
      (token ^. tokenExpr)

instance Show TMPParser where
  show :: TMPParser -> String
  show token =
    printf
      "%s                          \n\
      \%s %s =                     \n\
      \%s                          \n"
      ( createDefinition
          (toTmpName (token ^. name))
          (token ^. tokenType)
          (token ^. returnType)
      )
      (toTmpName (token ^. name))
      tmpCurrent
      (genericJoin "\n  <|>\n" (token ^. options))

instance Show TMPCommonParser where
  show :: TMPCommonParser -> String
  show parser =
    printf
      "{-# LANGUAGE TemplateHaskell #-}                                \n\
      \-- | Before block                                               \n\
      \%s                                                              \n\
      \                                                                \n\
      \import Control.Applicative ((<|>))                              \n\
      \import Lab4.Parser.Combinator (satisfy, nothing)                \n\
      \import Lab4.Parser.Parser (Parser (..))                         \n\
      \import Control.Lens (makeLenses, (&))                           \n\
      \                                                                \n\
      \-- parser produced by Sheol Version 1.0.0                       \n\
      \%s                                                              \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parsers                                          \n\
      \errorEndPoint :: Parser Token a                                 \n\
      \errorEndPoint = Parser sheolError                               \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parser                                           \n\
      \%s x = (runParser ((^. value) <$> (%s x))) . %s                 \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (parser ^. doBefore)
      (show (parser ^. attribute))
      ( join
          "\n"
          ( zip [1 :: Int ..] (parser ^. tokens)
              <&> _1 %~ toTmpTokenName
              <&> _2 %~ show
              <&> (^. each)
          )
      )
      ((parser ^. tmpParsers & each . options . each . variables . each %~ (find (parser ^. tokens ^.. each . pattern)) & each . tokenType .~ (parser ^. tokenName) & each %~ show) ^. each)
      (parser ^. parserName)
      (toTmpName (parser ^. tmpParsers ^? _head ^?! _Just ^. name))
      (parser ^. lexerName)
      (parser ^. doAfter)

find :: [String] -> TMPContext -> TMPContext
find token cotex = case (cotex ^. variable) `elemIndex` token of
  Just n -> cotex & variable .~ (toTmpTokenName (n + 1))
  Nothing -> cotex & variable %~ (toTmpName)
