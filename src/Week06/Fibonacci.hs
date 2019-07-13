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
  , interleaveStreams
  , streamFromSeed
  , nats
  , ruler
  , fibs3
  , fib4
  )
where

import           Data.Bits

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 = map snd $ iterate (\(a, b) -> (a + b, a)) (1, 0)

--------------------------- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) s2 = Cons a1 $ interleaveStreams s2 s1

-- interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (Cons a1 s1) ~(Cons a2 s2) =
--   Cons (a1) $ Cons (a2) $ (interleaveStreams s1 s2)

--------------------------- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- evenPowersOf2 :: Stream Integer
-- evenPowersOf2 =
--   streamMap floor
--     $ streamMap (\n -> log n / log 2)
--     $ streamMap realToFrac
--     $ streamMap (\n -> n .&. (complement (n - 1)) :: Integer)
--     $ streamFromSeed (+ 2) 2

-- ruler :: Stream Integer
-- ruler = interleaveStreams (streamRepeat 0) evenPowersOf2

ruler :: Stream Integer
ruler = interleaveIncrement $ streamRepeat 0 where
  interleaveIncrement (Cons a s) = interleaveStreams s (interleaveIncrement(streamRepeat (a + 1)))

--------------------------- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (Cons a as') + (Cons b bs') = Cons (a + b) (as' + bs')
  (Cons a as') * bs@(Cons b bs') =
    Cons (a * b) (streamMap (* a) bs' + as' * bs)

instance Fractional (Stream Integer) where
  as@(Cons a as') / bs@(Cons b bs') =
    Cons (a `div` b) (streamMap (`div` b) (as' - (as / bs) * bs'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

--------------------------- Exercise 7

data Matrix = Matrix (Integer, Integer) (Integer, Integer)

instance Num Matrix where
  (Matrix (a1, a2) (b1, b2)) * (Matrix (c1, c2) (d1, d2)) = Matrix
    (a1 * c1 + a2 * d1, a1 * c2 + a2 * d2)
    (b1 * c1 + b2 * d1, b1 * c2 + b2 * d2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case (Matrix (1, 1) (1, 0)) ^ n of
  Matrix (_, f) _ -> f
