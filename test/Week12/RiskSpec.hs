module Week12.RiskSpec
  ( spec
  , hspec
  ) where

import Control.Monad.Random (evalRandIO, evalRand, replicateM)
import Control.Monad.IO.Class (liftIO)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (MonadGen(..), (===), assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Random (StdGen, mkStdGen)
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe, shouldSatisfy)
import Week12.Risk (Battlefield(..), battle, invade, successProb)

spec :: Spec
spec =
  describe "Week 12" $ do
    battleSpec
    invadeSpec
    knownOutcomesSpec
    successProbSpec

unitsOnBattlefield :: Battlefield -> Int
unitsOnBattlefield bf = attackers bf + defenders bf

gameOver :: Battlefield -> Bool
gameOver (Battlefield as ds) = as < 2 || ds == 0

battleSpec :: Spec
battleSpec =
  describe "battle :: Battlefield -> Rand StdGen Battlefield" $ do
    it "battles with attacking army size of 1 should finish without any defensive loses" $
      require $
      property $ do
        defendingArmySize <- forAll $ Gen.int (Range.linear 0 100)
        let bf = Battlefield 1 defendingArmySize
        result <- liftIO (evalRandIO (battle bf))
        result === bf
    it "battles with defending army size of 0 should finish without any attacking loses" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 0 100)
        let bf = Battlefield attackingArmySize 0
        result <- liftIO (evalRandIO (battle bf))
        result === bf
    it "a fought battle reduces total battlefield units by the correct number" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 2 100)
        defendingArmySize <- forAll $ Gen.int (Range.linear 1 100)
        let expectedUnitDelta = if gameOver (Battlefield attackingArmySize defendingArmySize)
            then 0
            else min (min 3 (attackingArmySize - 1)) (min 2 defendingArmySize)
        let initialBattlefield = Battlefield attackingArmySize defendingArmySize
        newBattlefield <- liftIO (evalRandIO (battle initialBattlefield))
        unitsOnBattlefield newBattlefield === unitsOnBattlefield initialBattlefield - expectedUnitDelta

attackersTakeField :: Battlefield -> Bool
attackersTakeField (Battlefield as ds) = as > 1 && ds == 0

defendersTakeField :: Battlefield -> Bool
defendersTakeField (Battlefield as ds) = as < 2 && ds > 0

invadeSpec :: Spec
invadeSpec =
  describe "invade :: Battlefield -> Rand StdGen Battlefield" $ do
    it "invasion with attacking army size of 1 should finish without any defensive loses" $
      require $
      property $ do
        defendingArmySize <- forAll $ Gen.int (Range.linear 0 100)
        let bf = Battlefield 1 defendingArmySize
        result <- liftIO (evalRandIO (invade bf))
        result === bf
    it "invasion with defending army size of 0 should finish without any attacking loses" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 0 100)
        let bf = Battlefield attackingArmySize 0
        result <- liftIO (evalRandIO (invade bf))
        result === bf
    it "invasion with battles always finishes with either no defenders or less than 2 attackers" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 2 1000)
        defendingArmySize <- forAll $ Gen.int (Range.linear 1 1000)
        let initialBattlefield = Battlefield attackingArmySize defendingArmySize
        finalBattlefield <- liftIO (evalRandIO (invade initialBattlefield))
        assert (defendersTakeField finalBattlefield || attackersTakeField finalBattlefield)

knownOutcomesSpec :: Spec
knownOutcomesSpec =
  context "known outcome tests, controlling random generation seed" $ do
    describe "battle :: Battlefield -> Rand StdGen Battlefield" $ do
      it "random seed = 1, then Battlefield 12 10 => Battlefield 10 10" $
        evalRand (battle (Battlefield 12 10)) (mkStdGen 1) `shouldBe` Battlefield 10 10
      it "random seed = 17, then Battlefield 65 59 => Battlefield 65 57" $
        evalRand (battle (Battlefield 65 59)) (mkStdGen 17) `shouldBe` Battlefield 65 57
      it "random seed = 456, then Battlefield 546 550 => Battlefield 545 549" $
        evalRand (battle (Battlefield 546 550)) (mkStdGen 456) `shouldBe` Battlefield 545 549
    describe "invade :: Battlefield -> Rand StdGen Battlefield" $ do
      it "random seed = 1, then Battlefield 12 10 => Battlefield 1 5" $
        evalRand (invade (Battlefield 12 10)) (mkStdGen 1) `shouldBe` Battlefield 1 5
      it "random seed = 17, then Battlefield 65 59 => Battlefield 31 0" $
        evalRand (invade (Battlefield 65 59)) (mkStdGen 17) `shouldBe` Battlefield 31 0
      it "random seed = 456, then Battlefield 546 550 => Battlefield 8 0" $
        evalRand (invade (Battlefield 546 550)) (mkStdGen 456) `shouldBe` Battlefield 8 0

compareTwo :: Double -> Double -> Double -> Bool
compareTwo a b t = (abs (a - b)) <= t

allWithinTolerance ::  Double -> [Double] -> Bool
allWithinTolerance _ [] = True
allWithinTolerance _ [_] = True
allWithinTolerance t (x:y:xs) = compareTwo x y t && allWithinTolerance t xs

successProbSpec :: Spec
successProbSpec =
  describe "successProb :: Battlefield -> Rand StdGen Double" $ do
    it "calculates probability of 0 for no attackers" $ do
      let bf = Battlefield 0 10
      result <- liftIO (evalRandIO (successProb bf))
      result `shouldBe` 0
    it "calculates probability of 1 for no defenders" $ do
      let bf = Battlefield 10 0
      r <- liftIO (evalRandIO (successProb bf))
      r `shouldBe` 1
    it "calculates probabliity between 0.5 and 1.0 for larger attacking army" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 8 10)
        defendingArmySize <- forAll $ Gen.int (Range.linear 2 4)
        let bf = Battlefield attackingArmySize defendingArmySize
        result <- liftIO (evalRandIO (successProb bf))
        assert (result > 0.5 && result <= 1.0)
    it "calculates probabliity less than 0.5 for larger defending army" $
      require $
      property $ do
        attackingArmySize <- forAll $ Gen.int (Range.linear 2 4)
        defendingArmySize <- forAll $ Gen.int (Range.linear 8 10)
        let bf = Battlefield attackingArmySize defendingArmySize
        result <- liftIO (evalRandIO (successProb bf))
        assert (result < 0.5)
    it "repeated probably calcs are withing 0.15 of each other" $ do
      let (attackingArmySize, defendingArmySize) = (23, 21)
      let bf = Battlefield attackingArmySize defendingArmySize
      runs <- liftIO $ evalRandIO $ replicateM 5 (successProb bf)
      runs `shouldSatisfy` allWithinTolerance 0.15
