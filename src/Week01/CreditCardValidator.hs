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
toDigitsRev x 
  | x <= 0 = []
  | otherwise = mod x 10 : toDigitsRev (div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEverySecond . reverse
  where
    doubleEverySecond :: [Integer] -> [Integer]
    doubleEverySecond[] = [] 
    doubleEverySecond (x:[]) = x : []
    doubleEverySecond (x:(y:ys)) = x : y * 2 : doubleEverySecond ys 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
