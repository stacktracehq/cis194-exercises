module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  ) where

import Control.Arrow ((***), (&&&))
import Data.Array (accumArray, assocs)
import Data.List (transpose)

interval :: Integral a => a -> [b] -> [b]
interval n = map snd . filter ((0 ==) . (`mod` n) . fst) . zip [1 ..]

skips :: [a] -> [[a]]
skips = zipWith interval ([1 .. ] :: [Integer]) . (flip replicate <*> length)

neighbors :: [a] -> [((a, a), (a, a))]
neighbors = uncurry zip . ((zip <*> drop 1) &&& (zip . drop 1 <*> drop 2))

localMaxima :: Ord a => [a] -> [a]
localMaxima =
  map (snd . fst) .
  filter (uncurry (&&) . (uncurry (<) *** uncurry (>))) .
  neighbors

histogram :: [Integer] -> String
histogram ns =
  unlines $
  reverse $
  transpose $
  map toBar $
  counts ns
  where
    highestScore = maximum (map snd (counts ns))
    toBar (number, count) = show number ++ "=" ++ replicate count '*' ++ replicate (highestScore - count) ' '
    counts = assocs . accumArray (+) 0 (0, 9) . (`zip` repeat 1)
