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
import Data.Bifunctor (first)
import Data.Functor (void)

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
  fmap aTob (Parser a) = Parser $ (first aTob <$>) . a

---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser f) (Parser g) =
    Parser $ \s -> case f s of
      Nothing -> Nothing
      Just (aTob, rest) -> case g rest of
        Nothing -> Nothing
        Just (a, rest') -> Just (aTob a, rest')

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) (Parser f) (Parser g) =
  --   Parser $ \s -> do
  --     (aTob, rest) <- f s
  --     (a, rest') <- g rest

  --     return (aTob a, rest')

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Int]
intPair = combine <$> posInt <* char ' ' <*> posInt
    where combine a b = [fromInteger a, fromInteger b] 

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> f s <|> g s

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
