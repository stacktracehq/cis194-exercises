module Week01.CreditCardValidatorSpec
  ( spec
  , hspec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Week01.CreditCardValidator
  ( doubleEveryOther
  , doubleEveryOther
  , sumDigits
  , toDigits
  , toDigitsRev
  , validate
  )

spec :: Spec
spec =
  describe "CreditCardValidator" $ do
    toDigitsSpec
    toDigitsRevSpec
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

toDigitsRevSpec :: Spec
toDigitsRevSpec =
  describe "toDigitsRev" $
    it "does what toDigits does in reverse" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]
      toDigitsRev 0 `shouldBe` []
      toDigitsRev (-17) `shouldBe` []

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
