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
  , sieveSundaram
  ) where
import Control.Monad (join)
import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import Data.List ((\\))

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Maybe Integer
height Leaf = Nothing
height (Node h _ _ _) = Just h

setNodeHeight :: Tree a -> Tree a
setNodeHeight (Node _ l c r) = Node (calcHeight (l, r)) l c r
  where
    mapTuple = join (***)
    calcHeight = (+1) . fromMaybe 0 . uncurry max . mapTuple height
setNodeHeight t = t

insertNode :: a -> Tree a -> Tree a
insertNode v Leaf = Node 0 Leaf v Leaf
insertNode v (Node h l c r)
  | height l <= height r = setNodeHeight $ Node h (insertNode v l) c r
  | otherwise = setNodeHeight $ Node h l c (insertNode v r)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

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
xor = foldr (/=) False

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

-- impl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (flip f)

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ calcNonPrimes n'
  where
    n' = n `div` 2
    calcNonPrimes n'' = [i + j + 2 * i * j | i <- [1..n''], j <- [i..n'']]
