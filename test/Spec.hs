module Main
  ( main
  ) where

import Test.Hspec (hspec, Spec)
import qualified Week01.CreditCardValidatorSpec
import qualified Week02.LogAnalysisSpec

spec :: Spec
spec =  do
  Week01.CreditCardValidatorSpec.spec
  Week02.LogAnalysisSpec.spec

main :: IO ()
main = hspec spec
