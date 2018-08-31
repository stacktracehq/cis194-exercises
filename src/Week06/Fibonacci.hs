{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week06.Fibonacci
  ( fib
  , fibs1
  , fibs2
  , Stream(..)
  , streamToList
  , streamRepeat
  , streamMap
  , streamFromSeed
  , nats
  , ruler
  , fibs3
  , fib4
  ) where

import Data.List

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 = unfoldr nextFib (0, 1)
  where nextFib (a, b) = Just (a, (b, a + b))

--------------------------- Exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 101 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = Cons <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

--------------------------- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a0 a') b = Cons a0 (interleaveStreams b a')

ruler :: Stream Integer
ruler = f 0 where f a = interleaveStreams (streamRepeat a) (f (a + 1))

--------------------------- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (0 -)
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (streamMap (a0 *) b' + (a' * b))

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) (fromInteger (1 `div` b0) * (a' - (a / b) * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Int))

--------------------------- Exercise 7

newtype Matrix = Matrix [[Integer]]

instance Num Matrix where
  (*) (Matrix a) (Matrix b) = Matrix (map (\ai -> map (vectorMultiply ai) b') a)
    where b' = transpose b
          vectorMultiply c d = sum (zipWith (*) c d)

instance Show Matrix where
  show (Matrix a) = show a

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = 
  case Matrix [[1, 1], [1, 0]] ^ n of
    (Matrix [[_, b],_]) -> b
    _ -> error "Not possible"