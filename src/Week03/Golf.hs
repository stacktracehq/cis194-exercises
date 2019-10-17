module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

skips :: [a] -> [[a]]
skips xs = map (`everyNth` xs) [1 .. length xs]
  where everyNth n = map snd . filter ((== 0) . (`mod` n) . fst) . zip [1 ..]

localMaxima :: [Integer] -> [Integer]
localMaxima =
  let triples xs = zip3 xs (drop 1 xs) (drop 2 xs)
  in  map (\(_, n, _) -> n) . filter (\(a, n, b) -> n > a && n > b) . triples

histogram :: [Integer] -> String
histogram xs =
  let buildRows fs = map (\i -> buildRow i fs) (reverse [1 .. (maximum fs)])
      buildRow i = map (\f -> if f >= i then '*' else ' ')
      frequencies = map (\i -> (frequencyOf i xs)) [0 .. 9]
      frequencyOf n = fromIntegral . length . filter (== n)

      rows      = buildRows frequencies
      separator = replicate 10 '='
      legend    = concatMap show [0 .. 9]
  in  unlines $ rows ++ [separator, legend]

