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

import Data.List ((\\), nub)
---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = 
  foldl (\s x -> (x-2) * s) 1 . 
  filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = 
  sum . 
  filter even . 
  takeWhile (/=1) . 
  iterate (\n -> if even n then (n `div` 2) else (3 * n + 1))

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _ ) = h

setNodeHeight :: Tree a -> Tree a
setNodeHeight (Node _ lt y rt) = Node (calcHeight lt rt) lt y rt
  where
    calcHeight lt' rt'
      | treeHeight lt' <= treeHeight rt' = (+1) $ max 0 $ treeHeight rt'
      | otherwise                        = (+1) $ max 0 $ treeHeight lt'
setNodeHeight x = x

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h lt y rt) 
  | treeHeight lt <= treeHeight rt  = setNodeHeight $ Node h (insert x lt) y rt
  | otherwise                       = setNodeHeight $ Node h lt y (insert x rt)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf . reverse

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
xor = odd . length . filter (== True)

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

-- impl using foldr
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f b as = foldr (flip f) b as 

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2*x + 1)
  $ ([1..n] \\)
  $ nub
  $ map (\(i,j) -> i + j + 2*i*j)
  $ filter (\(i,j) -> (i <= j) && (i + j + 2*i*j <= n))
  $ cartProd [1..n] [1..n]