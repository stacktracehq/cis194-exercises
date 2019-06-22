module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  ) where

-- skipBy :: [a] -> Int -> [a]
-- skipBy as s = case (drop s as) of 
--   [] -> []
--   (x:xs) -> x : skipBy xs s

skipBy :: [a] -> Int -> [a]
skipBy as s = map (snd) $ filter (\n -> (fst n) `mod` s == 0) $ zip [1..length as] as

skips :: [a] -> [[a]]
skips as = map (skipBy as) [1..length as]

-- localMaxima :: [Integer] -> [Integer]
-- localMaxima xs@(a:b:c:_)
--   | b > a && b > c = b : localMaxima (drop 1 xs)
--   | otherwise = localMaxima (drop 1 xs)
-- localMaxima _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_,b,_) -> b) $ filter (\(a,b,c) -> b > a && b > c) $ zip3 xs (drop 1 xs) (drop 2 xs)

counts :: [Integer] -> [Int]
counts xs = [count x xs | x <- [0..9]] where
  count :: Integer -> [Integer] -> Int
  count v = length . filter (== v) 

drawHist :: [Int] -> String
drawHist xs = unlines $ reverse $ map (drawHistLine xs) [1..(maximum xs)] where
  drawHistLine :: [Int] -> Int -> String
  drawHistLine cs l = [if c >= l then '*' else ' ' | c <- cs]

histogram :: [Integer] -> String
histogram xs = (drawHist $ counts xs) ++ "==========\n0123456789\n"
