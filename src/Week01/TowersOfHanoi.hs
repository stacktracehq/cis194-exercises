module Week01.TowersOfHanoi
  ( hanoi
  ) where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)