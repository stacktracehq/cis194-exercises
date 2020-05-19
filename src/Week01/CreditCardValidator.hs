module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

toDigits :: Integer -> [Integer]
toDigits x 
  | x <= 0 = []
  | otherwise = map (\y -> read (y:[])) (show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | xs == [] = [x]
  | length xs `mod` 2 == 0 = x : doubleEveryOther xs
  | otherwise = x * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sum (map toDigits xs))

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0