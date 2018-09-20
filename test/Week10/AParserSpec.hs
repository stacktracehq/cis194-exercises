module Week10.AParserSpec
  ( spec
  , hspec
  ) where

import Data.Char (ord, toUpper)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (MonadGen(..), (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, hspec, it)
import Week10.AParser
  ( Parser(..)
  , abParser
  , abParser_
  , char
  , intOrUppercase
  , intPair
  )

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}
{-# ANN module ("HLint: ignore Functor law" :: String) #-}

spec :: Spec
spec =
  describe "Week 10" $ do
    functorSpec
    applicativeSpec
    abParserSpec
    abParser_Spec
    intPairSpec
    intOrUppercaseSpec

unicodeString :: MonadGen m => m String
unicodeString = Gen.string (Range.linear 0 100) Gen.unicode

functorSpec :: Spec
functorSpec =
  describe "Functor" $ do
    it "preserves identity" $
      require $
      property $ do
        c <- forAll Gen.unicode
        s <- forAll unicodeString
        runParser (id (char c)) s === runParser (fmap id (char c)) s

    it "preserves composition" $
      require $
      property $ do
        c <- forAll Gen.unicode
        s <- forAll unicodeString
        runParser ((fmap ord . fmap toUpper) (char c)) s ===
          runParser (fmap (ord . toUpper) (char c)) s

applicativeSpec :: Spec
applicativeSpec =
  describe "Applicative" $ do
    it "preserves identity" $
      require $
      property $ do
        c <- forAll Gen.unicode
        s <- forAll unicodeString
        runParser (id (char c)) s === runParser (pure id <*> char c) s

    it "preserves composition" $
      require $
      property $ do
        c <- forAll Gen.unicode
        s <- forAll unicodeString
        runParser (pure (.) <*> pure ord <*> pure toUpper <*> char c) s ===
          runParser (pure ord <*> (pure toUpper <*> char c)) s

    it "is homomorphic" $
      require $
      property $ do
        c <- forAll Gen.unicode
        s <- forAll unicodeString
        runParser (pure toUpper <*> pure c) s === runParser (pure (toUpper c)) s

abParserSpec :: Spec
abParserSpec =
  describe "abParser" $
    it "expects to see the characters 'a' and 'b' and returns them as a pair" $
      require $
      property $ do
        s <- forAll unicodeString
        runParser abParser ("ab" ++ s) === Just (('a','b'), s)
        runParser abParser (dropWhile ('a' ==) s) === Nothing

abParser_Spec :: Spec
abParser_Spec =
  describe "abParser_" $
    it "expects to see the characters 'a' and 'b' and returns ()" $
      require $
      property $ do
        s <- forAll unicodeString
        runParser abParser_ ("ab" ++ s) === Just ((), s)
        runParser abParser_ (dropWhile ('a' ==) s) === Nothing

intPairSpec :: Spec
intPairSpec =
  describe "intPair" $
    it "reads two integer values separated by a space and returns the integer values in a list" $
      require $
      property $ do
        n <- forAll $ Gen.int (Range.linear 0 100)
        m <- forAll $ Gen.int (Range.linear 0 100)
        runParser intPair (unwords (map show [n, m])) === Just ([n, m], "")

intOrUppercaseSpec :: Spec
intOrUppercaseSpec =
  describe "intOrUppercase" $
    it "parses either an integer value or an uppercase character, and fails otherwise" $
      require $
      property $ do
        n <- forAll $ Gen.int (Range.linear 0 100)
        c <- forAll Gen.upper
        lower <- forAll $ Gen.string (Range.linear 0 100) Gen.lower
        upper <- forAll $ Gen.string (Range.linear 0 100) Gen.upper
        runParser intOrUppercase (show n ++ lower) === Just ((), lower)
        runParser intOrUppercase (show n ++ upper) === Just ((), upper)
        runParser intOrUppercase (c : lower) === Just ((), lower)
        runParser intOrUppercase (c : upper) === Just ((), upper)
