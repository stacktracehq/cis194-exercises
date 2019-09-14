module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther'
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

import           Data.List                      ( unfoldr )

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  let f b | b <= 0    = Nothing
          | otherwise = Just (b `mod` 10, b `div` 10)
  in  unfoldr f n

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' =
  let doubleFromStart = zipWith ($) (cycle [id, (* 2)])
  in  reverse . doubleFromStart . reverse

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns =
  let getFn pred' = if pred' then (2 *) else id
      foldFn n (pred', ns') = (not pred', getFn pred' n : ns')
  in  snd $ foldr foldFn (False, []) ns

sumDigits :: [Integer] -> Integer
sumDigits = sum . (toDigits =<<)

validate :: Integer -> Bool
validate = (== 0) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
