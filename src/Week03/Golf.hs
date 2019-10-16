module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

import qualified Data.List.HT                  as HT
import           Data.List                      ( unfoldr )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Map                      as Map

skips :: [a] -> [[a]]
skips as =
  let sieve n = HT.sieve n . drop (n - 1)
      f n | n > length as = Nothing
          | otherwise     = Just (sieve n as, n + 1)
  in  unfoldr f 1

localMaxima :: [Integer] -> [Integer]
localMaxima ns =
  let secondIfBiggest (fst', snd', thd')
        | fst' < snd' && snd' > thd' = Just snd'
        | otherwise                  = Nothing
  in  mapMaybe secondIfBiggest $ zip3 ns (drop 1 ns) (drop 2 ns)

histogram :: [Integer] -> String
histogram = histogram' . (fromIntegral <$>)

-- I wanted to try to implement this avoiding transpose, and using map
-- just for the fun of it, might not be in the spirit of "golf", but oh well.
histogram' :: [Int] -> String
histogram' ns =
  let
    totals   = foldr (\n m -> Map.insertWith (const (+ 1)) n 1 m) Map.empty ns
    maxTotal = maximum $ Map.elems totals
    maxInput = max (maximum ns) 9
    xAxis    = [0 .. maxInput]
    starOrSpace n k = if n <= Map.findWithDefault 0 k totals then '*' else ' '
    line (n, ks) = starOrSpace n <$> ks
    separator = replicate (maxInput + 1) '='
    xLabels   = show =<< xAxis
    rowData   = zip (reverse [1 .. maxTotal]) (replicate maxTotal xAxis)
  in
    unlines $ (line <$> rowData) ++ [separator, xLabels]
