module Week04.Soln
  ( fun1
  , fun2
  , fun1'
  , fun2'
  , Tree(..)
  , foldTree
  , xor
  , map'
  , myFoldl
  , cartProd
  , sieveSundaram
  ) where

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = error "Week04.Soln#fun2' not implemented"

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = error "Week04.Soln#fun2' not implemented"

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = error "Week04.Soln#foldTree not implemented"

---------------------------  Exercise 3

xor :: [Bool] -> Bool
xor = error "Week04.Soln#xor not implemented"

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' = error "Week04.Soln#map' not implemented"

-- impl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = error "Week04.Soln#myFoldl not implemented"

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = error "Week04.Soln#sieveSundaram not implemented"
