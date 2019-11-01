module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

import Data.Bifunctor (second)
import Data.List (transpose)

skips :: [a] -> [[a]]
skips = 
  let
    replicateItAll :: [a] -> [[a]]
    replicateItAll as = replicate (length as) as

    filterItAll :: (Int, [a]) -> [a]
    filterItAll (i, word) = filterWithIndex ((== 0) . (`mod` i) . fst) word

    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [1..]

    mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
    mapWithIndex f = map f . zipWithIndex

    filterWithIndex :: ((Int, a) -> Bool) -> [a] -> [a]
    filterWithIndex f = map snd . filter f . zipWithIndex
  in
    mapWithIndex filterItAll . replicateItAll

localMaxima :: [Integer] -> [Integer]
localMaxima = 
  let
    zipBeforeAndAfter :: a -> [a] -> [(a, a, a)]
    zipBeforeAndAfter a xs = zip3 (drop 1 xs) xs (a : xs)

    isLocalMaxima :: (Integer, Integer, Integer) -> Bool
    isLocalMaxima (a, b, c) = b > a && b > c

    getMiddle :: (Integer, Integer, Integer) -> Integer
    getMiddle (_, x, _) = x
  in
    map getMiddle . filter isLocalMaxima . zipBeforeAndAfter 0

histogram :: [Integer] -> String
histogram =
  let
    columns :: [Integer] -> [String]
    columns = prepForOutput . map toColumn . countZeroToNine
      where
        countZeroToNine :: [Integer] -> [(Integer, Int)]
        countZeroToNine xs =
          let 
            count :: (a -> Bool) -> [a] -> Int
            count f = length . filter f
          in
            map (\x -> (x, count (== x) xs)) [0..9]

        toColumn :: (Integer, Int) -> String
        toColumn (x,y) =
          let
            duplicate :: Int -> String -> String
            duplicate i = concat . replicate i
          in
            duplicate y "*" ++ "=" ++ show x

        prepForOutput :: [String] -> [String]
        prepForOutput xs =
          let
            maxLength :: [String] -> Int
            maxLength = maximum . map length
            padLeft :: Int -> String -> String
            padLeft i s = replicate (i - length s) ' ' ++ s
          in
            map (padLeft $ maxLength xs) xs
  in
    unlines . transpose . columns
