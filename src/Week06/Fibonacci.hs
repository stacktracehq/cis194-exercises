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
  )
where

---------------------------  Exercise 1

fib :: Integer -> Integer
fib = error "Week06.Fibonacci#fib not implemented"

fibs1 :: [Integer]
fibs1 = error "Week06.Fibonacci#fibs1 not implemented"

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 = error "Week06.Fibonacci#fibs2 not implemented"

--------------------------- Exercise 3

data Stream a = ImplementMe

streamToList :: Stream a -> [a]
streamToList = error "Week06.Fibonacci#streamToList not implemented"

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = error "Week06.Fibonacci#streamRepeat not implemented"

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = error "Week06.Fibonacci#streamMap not implemented"

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = error "Week06.Fibonacci#streamFromSeed not implemented"

--------------------------- Exercise 5

nats :: Stream Integer
nats = error "Week06.Fibonacci#nats not implemented"

ruler :: Stream Integer
ruler = error "Week06.Fibonacci#ruler not implemented"

--------------------------- Exercise 6

fibs3 :: Stream Integer
fibs3 = error "Week06.Fibonacci#fibs3 not implemented"

--------------------------- Exercise 7

fib4 :: Integer -> Integer
fib4 = error "Week06.Fibonacci#fibs4 not implemented"
