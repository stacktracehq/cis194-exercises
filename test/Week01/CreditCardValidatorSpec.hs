module Week01.CreditCardValidatorSpec
  ( spec
  , hspec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Week01.CreditCardValidator
  ( doubleEveryOther
  , everyNth
  , sumDigits
  , toDigits
  , toDigits2
  , toDigits3
  , toDigitsRev
  , toDigitsRev2
  , validate
  )

spec :: Spec
spec =
  describe "CreditCardValidator" $ do
    toDigitsSpec
    toDigitsSpec2
    toDigitsSpec3
    toDigitsRevSpec
    toDigitsRevSpec2
    everyNthSpec
    doubleEveryOtherSpec
    sumDigitsSpec
    validateSpec

toDigitsSpec :: Spec
toDigitsSpec =
  describe "toDigits" $
    it "converts positive Integers to a list of digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []

toDigitsSpec2 :: Spec
toDigitsSpec2 =
  describe "toDigits2" $
    it "converts positive Integers to a list of digits" $ do
      toDigits2 1234 `shouldBe` [1,2,3,4]
      toDigits2 0 `shouldBe` []
      toDigits2 (-17) `shouldBe` []

toDigitsSpec3 :: Spec
toDigitsSpec3 =
  describe "toDigits3" $
    it "converts positive Integers to a list of digits" $ do
      toDigits3 1234 `shouldBe` [1,2,3,4]
      toDigits3 0 `shouldBe` []
      toDigits3 (-17) `shouldBe` []

toDigitsRevSpec :: Spec
toDigitsRevSpec =
  describe "toDigitsRev" $
    it "does what toDigits does in reverse" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]
      toDigitsRev 0 `shouldBe` []
      toDigitsRev (-17) `shouldBe` []

toDigitsRevSpec2 :: Spec
toDigitsRevSpec2 =
  describe "toDigitsRev2" $
    it "does what toDigits does in reverse" $ do
      toDigitsRev2 1234 `shouldBe` [4,3,2,1]
      toDigitsRev2 0 `shouldBe` []
      toDigitsRev2 (-17) `shouldBe` []

everyNthSpec :: Spec
everyNthSpec =
  describe "everyNth" $
    it "applies a function to every nth element of a list" $ do
      everyNth 3 (*2) [1,2,3,4,5,6] `shouldBe` ([1,2,6,4,5,12] :: [Int])
      everyNth 3 (*2) [1,2,3,4] `shouldBe` ([1,2,6,4] :: [Int])

doubleEveryOtherSpec :: Spec
doubleEveryOtherSpec =
  describe "doubleEveryOther" $
    it "doubles every second digit starting from the second last" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

sumDigitsSpec :: Spec
sumDigitsSpec =
  describe "sumDigits" $
    it "sums the sum of the digits of all numbers in a list" $
      -- [2,3,16,6] becomes 2 + 3 + 1 + 6 + 6 = 18
      sumDigits [16,7,12,5] `shouldBe` 22

validateSpec :: Spec
validateSpec =
  describe "validate" $
    it "indicates whether an Integer could be a valid credit card number" $ do
      -- See: http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False
