{-# LANGUAGE InstanceSigs #-}

module Lab4.Parser.Parser
  ( -- * `Parser` constructors
    Parser (..),
    (<|>)
  )
where

import Control.Applicative (Alternative (..))

newtype Parser s a = Parser
  { -- | starting the parser
    runParser :: [s] -> Maybe (a, [s])
  }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f p = Parser $ \input -> toB (runParser p input)
    where
      toB Nothing = Nothing
      toB (Just (x, xs)) = Just (f x, xs)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \input -> Just (a, input)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  p1 <*> p2 = Parser $ \input -> toB (runParser p1 input)
    where
      toB Nothing = Nothing
      toB (Just (f, xs)) = runParser (fmap f p2) xs

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  p >>= f = Parser $ \input -> toB (runParser p input)
    where
      toB Nothing = Nothing
      toB (Just (x, xs)) = runParser (f x) xs

instance Alternative (Parser s) where
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

  empty :: Parser s a
  empty = Parser $ const Nothing
