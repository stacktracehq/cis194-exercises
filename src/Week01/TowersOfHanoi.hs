module Week01.TowersOfHanoi
  ( hanoi
  )
where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoj :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoj 0 _ _ _ _ = []
hanoj 1 a b _ _ = [(a, b)]
hanoj n a b c1 c2 = hanoj (n - 2) a c1 c2 b ++ [(a, c2), (a, b), (c2, b)] ++ hanoj (n - 2) c1 b a c2