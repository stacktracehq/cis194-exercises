module Week03.GolfSpec
  ( spec
  , hspec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )

spec :: Spec
spec =
  describe "Golf" $ do
    skipsSpec
    localMaximaSpec
    histogramSpec

skipsSpec :: Spec
skipsSpec =
  describe "skips" $
    it "returns the correct list" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [1,2,3,4,5] `shouldBe` [[1,2,3,4,5], [2,4], [3], [4], [5]]
      skips [True,False] `shouldBe` [[True,False], [False]]
      skips [] `shouldBe` ([] :: [[Int]])

localMaximaSpec :: Spec
localMaximaSpec =
  describe "localMaxima" $
    it "finds all local maxima" $ do
      localMaxima [1,3,2] `shouldBe` [3]
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []
      localMaxima [6,6,6,6,7,7] `shouldBe` []
      localMaxima [8,8,7,7,6,7] `shouldBe` []

histogramSpec :: Spec
histogramSpec =
  describe "histogram" $
    it "returns the correct histogram representation" $ do
      histogram [0,2,4,6,8] `shouldBe` "* * * * * \n==========\n0123456789\n"
      histogram [1,3,5,7,9] `shouldBe` " * * * * *\n==========\n0123456789\n"
      histogram [1,1,1,5] `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789\n"
      histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
