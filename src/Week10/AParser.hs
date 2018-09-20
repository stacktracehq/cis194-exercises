{-# LANGUAGE InstanceSigs #-}

module Week10.AParser
  ( Parser(..)
  , satisfy
  , char
  , posInt
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  ) where

import Data.Functor (void)
import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isUpper)
import Data.Bifunctor (first)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

---------------------------  Exercise 1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (first f) . p

---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Just (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s') <- pf s
    (a, s'') <- pa s'
    return (f a, s'')

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

int :: Parser Int
int = fromInteger <$> posInt

intPair :: Parser [Int]
intPair = (\a b -> [a, b]) <$> int <*> (char ' ' *> int)

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser a) <|> (Parser b) = Parser $ \input ->
    a input <|> b input

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase =
  void posInt <|> void (satisfy isUpper)
