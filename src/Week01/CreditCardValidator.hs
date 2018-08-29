module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

import Data.List (unfoldr)
import Data.Tuple (swap)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev =
  unfoldr $ \n ->
    if n <= 0
      then Nothing
      else Just (swap (divMod n 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . (toDigits =<<)

validate :: Integer -> Bool
validate =
  (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
