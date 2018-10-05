{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Data.Ord (Down(Down))
import Control.Monad.Random
import Control.Arrow ((&&&))
import Data.List (reverse, sortOn)

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

attacking :: Battlefield -> Army
attacking battleField = min (max (attackers battleField - 1) 0) 3

defending :: Battlefield -> Army
defending battleField = min (defenders battleField) 2

battle :: Battlefield -> Rand StdGen Battlefield
battle battleField = do
  results <-
    zipWith (<=)
      <$> (sortOn Down <$> replicateM (attacking battleField) die)
      <*> (sortOn Down <$> replicateM (defending battleField) die)
  pure $ Battlefield
    (attackers battleField - length (filter id results))
    (defenders battleField - length (filter not results))

------------------------------------------------------------
-- Exercise 3

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v = pure v
  | otherwise = f v >>= iterateUntilM p f

invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM done battle
  where
    done battleField = defenders battleField < 1 || attackers battleField < 2

------------------------------------------------------------
-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb =
  ((/ 1000) . realToFrac . length . filter ((< 1) . defenders) <$>) .
  replicateM 1000 . invade

------------------------------------------------------------
-- Exercise 5

-- Anyone know probability theory :p

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = error "Week12.Risk#exactSuccessProb not implemented"
