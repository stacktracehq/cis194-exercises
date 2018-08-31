module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  ) where

import Data.List

everyN :: [a] -> Int -> [a]
everyN xs n = map snd (filter (\y -> fst y `mod` n == 0) (zip [1..(length xs)] xs))

skips :: [a] -> [[a]]
skips as = map (everyN as) [1..(length as)]


localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_,b,_) -> b) (filter (\(a,b,c) -> b > a && b > c) (zip3 xs (drop 1 xs) (drop 2 xs)))


count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

type Count = Int
type YPosition = Int
type Width = Int

histogramCounts :: Eq a => [a] -> [a] -> [(a, Count)]
histogramCounts xs domain = map (\x -> (x, count x xs)) (nub domain)

histogramWidths :: Show a => [(a, Count)] -> [Width]
histogramWidths xs = replicate (length xs) (maximum (map (length . show . fst) xs))

histogramData :: (Show a, Eq a) => [(a, Count)] -> [(a, Count, Width)]
histogramData xs = zip3 (map fst xs) (map snd xs) (histogramWidths xs)

histogramItem :: (Show a, Eq a) => YPosition -> (a, Count, Width) -> String
histogramItem y (x, c, width)
  | y == -1 = show x ++ replicate (width - length (show x)) ' '
  | y == 0 = replicate width '='
  | c >= y = '*' : replicate (width - 1) ' '
  | otherwise = replicate width ' '

histogramLine :: (Show a, Eq a) => YPosition -> [(a, Count, Width)] -> String
histogramLine y = concatMap (histogramItem y)

histogramStr :: (Show a, Eq a) => [(a, Count)] -> String
histogramStr xs =
  let
    ds = histogramData xs
    maxCount = maximum (map (\(_, c, _) -> c) ds)
  in concatMap (\y -> histogramLine y ds ++ "\n") [maxCount, (maxCount-1)..(-1)]

histogramWithDomain :: (Show a, Eq a) => [a] -> [a] -> String
histogramWithDomain xs domain = histogramStr (histogramCounts xs domain)

-- histogramSimple :: (Show a, Eq a) => [a] -> String
-- histogramSimple xs = histogramWithDomain xs xs

histogram :: [Integer] -> String
histogram xs = histogramWithDomain xs [0..9]
