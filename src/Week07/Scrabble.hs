{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Week07.Scrabble
  ( Score(..)
  , score
  , scoreString
  ) where

import Data.Char (toLower)

--------------------------- Exercise 3

newtype Score = Score { getScore :: Int }
  deriving (Show, Eq, Ord, Num)

instance Semigroup Score where
  (<>) a b = Score (getScore a + getScore b)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c = case toLower c of
  'a' -> Score 1
  'b' -> Score 3
  'c' -> Score 3
  'd' -> Score 2
  'e' -> Score 1
  'f' -> Score 4
  'g' -> Score 2
  'h' -> Score 4
  'i' -> Score 1
  'j' -> Score 8
  'k' -> Score 5
  'l' -> Score 1
  'm' -> Score 3
  'n' -> Score 1
  'o' -> Score 1
  'p' -> Score 3
  'q' -> Score 10
  'r' -> Score 1
  's' -> Score 1
  't' -> Score 1
  'u' -> Score 1
  'v' -> Score 4
  'w' -> Score 4
  'x' -> Score 8
  'y' -> Score 4
  'z' -> Score 10
  _ -> mempty

scoreString :: String -> Score
scoreString = foldr ((<>) . score) mempty
