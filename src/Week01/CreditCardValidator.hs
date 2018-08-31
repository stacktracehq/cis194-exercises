module Week01.CreditCardValidator
  ( toDigits
  , toDigits2
  , toDigits3
  , toDigitsRev
  , toDigitsRev2
  , doubleEveryOther
  , sumDigits
  , validate
  , everyNth
  ) where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigits2 :: Integer -> [Integer]
toDigits2 = reverse . toDigitsRev2

toDigits3 :: Integer -> [Integer]
toDigits3 n
  | n <= 0 = []
  | otherwise = map (fromIntegral . digitToInt) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

toDigitsRev2 :: Integer -> [Integer]
toDigitsRev2 n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev2 (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . everyNth 2 (*2) . reverse

everyNth ::  Int -> (a -> a) -> [a] -> [a]
everyNth n f = zipWith ($) (cycle (replicate (n - 1) id ++ [f]))

sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

validate :: Integer -> Bool
validate = (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
