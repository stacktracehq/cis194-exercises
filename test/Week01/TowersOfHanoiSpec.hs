module Week01.TowersOfHanoiSpec
  ( towersOfHanoiSpec
  ) where

import Week01.TowersOfHanoi (hanoi)
import Test.Hspec (describe, it, shouldBe, Spec)

towersOfHanoiSpec :: Spec
towersOfHanoiSpec =
  describe "TowersOfHanoi"
    hanoiSpec

hanoiSpec :: Spec
hanoiSpec =
  describe "hanoi" $
    it "lists the moves needed to move _n_ disks from peg a to peg b" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
