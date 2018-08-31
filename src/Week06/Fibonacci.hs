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

import Data.List (unfoldr)

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)


fibs1 :: [Integer]
fibs1 = map fib [0..]

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 = unfoldr (Just . nextFib) (0, 1)
  where
    nextFib a = (fst a, (snd a, uncurry (+) a))

--------------------------- Exercise 3

data Stream a = a :+ Stream a

streamToList :: Stream a -> [a]
streamToList (a :+ as) = a : streamToList as

infixr 5 :+

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = (:+) <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a :+ as) = f a :+ streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a :+ streamFromSeed f (f a)

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (x :+ xs) ys = x :+ streamInterleave ys xs

--------------------------- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = interleaveIncrement $ streamRepeat 0
  where
    repeatIncrement = streamRepeat . (+ 1)
    interleaveIncrement (x :+ xs) = streamInterleave xs (interleaveIncrement $ repeatIncrement x)

--------------------------- Exercise 6

x :: Stream Integer
x = 0 :+ 1 :+ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger = (:+ streamRepeat 0)
  negate = streamMap negate
  (+) (a :+ as) (b :+ bs) = a + b :+ as + bs
  (*) (a0 :+ a') b@(b0 :+ b') = a0 * b0 :+ streamMap (* a0) b' + a' * b

instance Fractional (Stream Integer) where
  (/) a@(a0 :+ a') b@(b0 :+ b') = (a0 `div` b0) :+ streamMap (`div` b0) (a' - a / b * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

--------------------------- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a0 b0 c0 d0) (Matrix a1 b1 c1 d1) =
    Matrix (a0 * a1 + b0 * c1) (a0 * b1 + b0 * d1) (c0 * a1 + d0 * c1) (c0 * b1 + d0 * d1)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (pluckFib . toPower) n
  where
    toPower = (Matrix 1 1 1 0 ^)
    pluckFib (Matrix _ fib _ _) = fib
