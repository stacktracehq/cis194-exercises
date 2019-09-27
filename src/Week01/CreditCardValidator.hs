module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0    = []
              | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . fromleft . reverse
 where
  fromleft :: [Integer] -> [Integer]
  fromleft (x : s : xs) = x : (2 * s) : (fromleft xs)
  fromleft xss          = xss


sumDigits :: [Integer] -> Integer
sumDigits x = sum $ concat $ map (\n -> toDigits n) x

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0
