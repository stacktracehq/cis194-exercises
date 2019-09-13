module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

-- reverse' :: [a] -> [a]
-- reverse' []       = []
-- reverse' (x : xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

divMod' :: Integer -> Integer -> (Integer, Integer)
divMod' n r = (n `div` r, n `mod` r)

toDigits :: Integer -> [Integer]
toDigits = reverse'' . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0    = []
              | otherwise = let (d, m) = divMod' x 10 in m : toDigitsRev d

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  error "Week01.CreditCardValidator#doubleEveryOther not implemented"

sumDigits :: [Integer] -> Integer
sumDigits = error "Week01.CreditCardValidator#sumDigits not implemented"

validate :: Integer -> Bool
validate = error "Week01.CreditCardValidator#validate not implemented"
