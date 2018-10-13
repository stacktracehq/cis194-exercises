{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.List (sortBy)

------------------------------------------------------------
-- Die values
newtype DieValue = DV
  { unDV :: Int
  } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  } deriving (Show, Eq)

------------------------------------------------------------
-- Exercise 1
-- This is just installing the monad random module, you don't
-- have to worry about this due to the magic of nix

------------------------------------------------------------
-- Exercise 2

roll :: Int -> Rand StdGen [DieValue]
roll = fmap sortDesc . flip replicateM die
  where
    sortDesc = sortBy (flip compare)

results :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [Battlefield -> Battlefield]
results as ds = zipWith decideKill <$> as <*> ds
  where
    decideKill a d
      | a > d = \b -> b { defenders = defenders b - 1 }
      | otherwise = \b -> b { attackers = attackers b - 1 }

battle :: Battlefield -> Rand StdGen Battlefield
battle b = foldr takeKills b <$> kills
  where
    takeKills f = f
    kills = results rollAttack rollDefense
    rollAttack = roll (min maxAttack (attackers b - 1))
    rollDefense = roll (min maxDefense (defenders b))
    maxAttack = 3
    maxDefense = 2

------------------------------------------------------------
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | (attackers b < 2) || (defenders b < 1) = pure b
  | otherwise = invade =<< battle b

------------------------------------------------------------
-- Exercise 4

successProbN :: Int -> Battlefield -> Rand StdGen Double
successProbN n = fmap (probN n) . invadeN n
  where
    invadeN = (. invade) . replicateM
    probN n' = (/ realToFrac n') . realToFrac . length . filter ((< 1) . defenders)

successProb :: Battlefield -> Rand StdGen Double
successProb = successProbN 1000

------------------------------------------------------------
-- Exercise 5

-- Anyone know probability theory :p

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = error "Week12.Risk#exactSuccessProb not implemented"
