module Main
  ( main
  ) where

import Test.Hspec (hspec, Spec)
import qualified Week01.CreditCardValidatorSpec
import qualified Week02.LogAnalysisSpec
import qualified Week03.GolfSpec
import qualified Week04.SolnSpec
import qualified Week05.CalcSpec

spec :: Spec
spec =  do
  Week01.CreditCardValidatorSpec.spec
  Week02.LogAnalysisSpec.spec
  Week03.GolfSpec.spec
  Week04.SolnSpec.spec
  Week05.CalcSpec.spec

main :: IO ()
main = hspec spec
