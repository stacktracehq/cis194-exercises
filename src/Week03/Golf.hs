module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

skipN :: Int -> [a] -> [a]
skipN _ [] = []
skipN n as = (\ys -> take 1 ys ++ skipN n (drop 1 ys)) . drop n $ as

skips :: [a] -> [[a]]
skips = map (\(n, a) -> skipN n a) . (\as -> zip [0 ..] (replicate (length as) as))

localMaxima :: [Integer] -> [Integer]
localMaxima = error "Week03.Golf#localMaxima not implemented"

histogram :: [Integer] -> String
histogram = error "Week03.Golf#histogram not implemented"