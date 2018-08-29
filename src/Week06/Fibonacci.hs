{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week06.Fibonacci
  ( fib
  , fibs1
  , fibs2
  , fibs2'
  , fibs2''
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

import Data.List (unfoldr)

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 =
  map fib [0 ..]

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 =
  0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

fibs2' :: [Integer]
fibs2' =
  unfoldr (\(a, b) -> Just (a, (b, a + b))) (0,1)

fibs2'' :: [Integer]
fibs2'' =
  0 : scanl (+) 1 fibs2''

--------------------------- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) =
  a : streamToList as

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a =
  streamFromSeed (const a) a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) =
  Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =
  Cons a (streamFromSeed f (f a))

--------------------------- Exercise 5

nats :: Stream Integer
nats =
  streamFromSeed (+ 1) 0

-- When right folding you can't force the accumulator and stay lazy.
-- Using `~` in front of a pattern makes it lazy, the match will only be performed
-- when a value from it is demanded.
-- This is sugar over performing the match in a `let` binding, which desugars to
-- performing it in a lambda ... you might see where this is going.
-- There are other (simpler) ways to defer this match per itereration ...
-- ask Lord Bow :P
-- You can write this without streamFold, and in a way that doesn't need
-- interleaveStream to be lazy in it's second arg ... but it's less elegant.
interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons a as) ~(Cons b bs) =
  Cons a (Cons b (interleaveStream as bs))

streamFold :: (a -> b -> b) -> Stream a -> b
streamFold f (Cons a as) =
  a `f` streamFold f as

ruler :: Stream Integer
ruler =
  streamFold (interleaveStream . streamRepeat) nats

--------------------------- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * bs@(Cons b bs') = Cons (a * b) (streamMap (* a) bs' + (as * bs))

instance Fractional (Stream Integer) where
  as@(Cons a as') / bs@(Cons b bs') = Cons (a `div` b) (streamMap (`div` b) (as' - (as / bs) * bs'))

fibs3 :: Stream Integer
fibs3 =
  x / (1 - x - x ^ (2 :: Integer))

--------------------------- Exercise 7

data Matrix2x2 = Matrix2x2
  { x1 :: Integer
  , y1 :: Integer
  , x2 :: Integer
  , y2 :: Integer
  }

matrixF :: Matrix2x2
matrixF = Matrix2x2 1 1 1 0

instance Num Matrix2x2 where
  a * b =
    Matrix2x2
      ((x1 a * x1 b) + (y1 a * x2 b))
      ((x1 a * y1 b) + (y1 a * y2 b))
      ((x2 a * x1 b) + (y2 a * x2 b))
      ((x2 a * y1 b) + (y2 a * y2 b))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = y1 (matrixF ^ n)
