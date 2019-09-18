module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- alternate impl #1
-- assumes toDigitsRev not defined in terms of toDigits
-- toDigits n = reverse (toDigitsRev n)

-- alternate impl #2
-- same as alernate #1, but point free
-- toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0    = []
              | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherL . reverse
 where
  doubleEveryOtherL []           = []
  doubleEveryOtherL [x         ] = [x]
  doubleEveryOtherL (x : y : ys) = x : 2 * y : doubleEveryOtherL ys

sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : xs) = (sumInts (toDigits x)) + sumDigits xs
 where
  sumInts []       = 0
  sumInts (y : ys) = y + sum ys

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
