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

import Data.List

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight (Node h _ _ _) = h
treeHeight Leaf = -1

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ l v r) =
  let
    (l', r') =
        if treeHeight l < treeHeight r
        then (treeInsert x l, r)
        else (l, treeInsert x r)
  in
    Node (max (treeHeight l') (treeHeight r') + 1) l' v r'

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

showTree :: Show a => Tree a -> String
showTree Leaf = ""
showTree n@(Node s _ _ _) = go s n
  where
    go _ Leaf = ""
    go i (Node h l c r) = go (i-1) l ++
      replicate (4*fromIntegral i) ' ' ++ show c ++ "-" ++ show h ++ "\n" ++ go (i-1) r

-- will print a tree in ghci, root node will be the rightmost in the printed structure
-- nodes will be printed as [value]-[height]
printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ showTree t

---------------------------  Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\x y -> (not x && y) || (x && not y)) False

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- impl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f seed xs = foldr (flip f) seed (reverse xs)

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ itemsToRemove)
  where itemsToRemove = (filter (<= n) . map (\(i, j) -> i + j + 2*i*j) . filter (uncurry (<=))) (cartProd [1..n] [1..n])
