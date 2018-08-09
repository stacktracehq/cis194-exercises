module Week04.Soln
  ( fun1
  , fun2
  , fun1'
  , fun2'
  , Tree(..)
  , foldTree
  , showTree
  , printTree
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

showTree :: Show a => Tree a -> String
showTree Leaf = ""
showTree n@(Node s _ _ _) = go s n
  where
  go _ (Leaf) = ""
  go i (Node h l c r) = go (i-1) l ++
    replicate (4*fromIntegral i) ' ' ++ show c ++ "-" ++ show h ++ "\n" ++ go (i-1) r

-- will print a tree in ghci, root node will be the rightmost in the printed structure
-- nodes will be printed as [value]-[height]
printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ showTree t

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
