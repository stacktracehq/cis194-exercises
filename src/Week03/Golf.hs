module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where


skips :: [a] -> [[a]]
skips items = map (\u -> takeEvery items u) [1..(length items)]
  where 
    takeEvery :: [a] -> Int -> [a]
    takeEvery [] _ = []
    takeEvery _ 0 = []
    takeEvery list n = map (\u -> fst(u)) (filter (\x -> (mod (snd(x)) (n)) == 0) (zip list [1..]))


-- localMaxima :: [Integer] -> [Integer]
-- localMaxima [] = []
-- localMaxima (_ : []) = []
-- localMaxima (_ : _ : []) = []
-- localMaxima (x : xs : xxs: xxxs) 
--   | x < xs && xs > xxs = xs : localMaxima (xs : xxs: xxxs)
--   |otherwise           =  localMaxima (xs : xxs: xxxs)


localMaxima :: [Integer] -> [Integer]
localMaxima items = map (\(_,b,_)->b) (filter (\(x,y,z)->x<y && y>z) (zip3 items (drop 1 items) (drop 2 items)))


histogram :: [Integer] -> String
histogram = error "Week03.Golf#histogram not implemented"
