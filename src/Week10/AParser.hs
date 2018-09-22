{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Arrow (first)
import Control.Monad (void, (<=<))

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

upper :: Parser Char
upper = satisfy isUpper

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
  fmap f p = Parser (mapResult f . runParser p)
    where
      mapResult = fmap . first

---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (Just . (a,))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  l <*> r = Parser (applyAtoB <=< runParser l)
    where
      applyAtoB = uncurry $ runParser . (<$> r)

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Int]
intPair = pair <$> posIntegral <* char ' ' <*> posIntegral
  where
    pair a b = [a, b]
    posIntegral = fromIntegral <$> posInt

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = Parser(\r -> runParser a r <|> runParser b r)

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void upper
