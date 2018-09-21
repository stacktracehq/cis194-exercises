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

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isUpper)
import Control.Monad

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Int
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

---------------------------  Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (fmap (first f) . runParser p)

---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Just (a, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pAToB <*> pA = Parser (\s ->
    do
      (aToB, s') <- runParser pAToB s
      (a, s'') <- runParser pA s'
      Just (aToB a, s''))

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Int]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  l <|> r = Parser (\s -> runParser l s <|> runParser r s)

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = int <|> upperCase
  where
    int = void posInt
    upperCase = void (satisfy isUpper)
