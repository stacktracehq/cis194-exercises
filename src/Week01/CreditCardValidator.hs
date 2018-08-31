module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 (a:as) (b:bs) = (a,b) : (zip2 as bs)
   
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = (reverse2 xs) ++ [x]

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = (f x) : (map2 f xs)

flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:xs) = flatten xs
flatten ((y:ys):xs) = y : (flatten (ys:xs))

sum2 :: [Integer] -> Integer
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

toDigits :: Integer -> [Integer]
toDigits x = 
  reverse2 (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | True = (x `mod` 10) : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  reverse2 (map2 (\(i, x) -> if i `mod` 2 == 0 then x else 2 * x) (zip2 ([0..]::[Int]) (reverse2 xs)))

sumDigits :: [Integer] -> Integer
sumDigits xs =
  sum2 (flatten (map2 toDigits xs))

validate :: Integer -> Bool
validate x =
  (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
