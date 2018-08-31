module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  ) where

import Data.List (sort, group, transpose)
import Data.Char (isSpace)

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

skips :: [a] -> [[a]]
skips l =  zipWith takeEvenyNth [1..] $ replicate (length l) l
  where
    isDivisibleBy n = (0==) . (`mod` n)
    takeEvenyNth n = map snd . filter (isDivisibleBy n . fst) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima l = map snd3 $ filter isSndMax $ zip3 l (drop 1 l) (drop 2 l)
  where
    isSndMax (a, b, c) = (a < b) && (b > c)

histogram :: [Integer] -> String
histogram
  = unlines
  . dropWhile (all isSpace)
  . reverse
  . transpose
  . footer
  . map (makeColumn . drop 1)
  . group
  . sort
  . (++ [0..9])
  where
    strPadRight a b cs = cs ++ replicate (b - length cs) a
    makeColumn = strPadRight ' ' 10 . map (const '*')
    footer = zipWith (\a b -> show a ++ "=" ++ b) [0..]
