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
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Data.Char                      ( isDigit )

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
 where
  f [] = Nothing
  f (x : xs) | p x       = Just (x, xs)
             | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
 where
  f xs | null ns   = Nothing
       | otherwise = Just (read ns, rest)
    where (ns, rest) = span isDigit xs

---------------------------  Exercise 1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = error "Week10.AParser#fmap not implemented"

---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure = error "Week10.AParser#pure not implemented"

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = error "Week10.AParser#(<*>) not implemented"

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = error "Week10.AParser#abParser not implemented"

abParser_ :: Parser ()
abParser_ = error "Week10.AParser#abParser_ not implemented"

intPair :: Parser [Int]
intPair = error "Week10.AParser#intPair not implemented"

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = error "Week10.AParser#empty not implemented"

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = error "Week10.AParser#(<|>) not implemented"

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = error "Week10.AParser#intOrUppercase not implemented"
