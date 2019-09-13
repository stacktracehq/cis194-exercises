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
toDigitsRev a
  | a <= 0 = []
  | otherwise = a `mod` 10 : toDigitsRev (a `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = 
  let
    accumulator x (True, list) = (False, (2*x) : list)
    accumulator x (False, list) = (True, x : list)
    (_, result) = foldr accumulator (False, []) xs
  in result

sumDigits :: [Integer] -> Integer
sumDigits = sum . (toDigits =<<)

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

