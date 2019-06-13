module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where
  
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10] 
      
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10) 
      
-- toDigits :: Integer -> [Integer]
-- toDigits n = reverse (toDigitsRev n)

-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev n
--   | n <= 0 = []
--   | otherwise = n `mod` 10 : toDigitsRev (n `div` 10) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where 
    doubleEveryOther' :: [Integer] -> [Integer]
    doubleEveryOther' (x:(y:ys)) = x : 2*y : doubleEveryOther' ys
    doubleEveryOther' x = x
    
sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
