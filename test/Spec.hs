module Main
  ( main
  ) where

import Test.Hspec (hspec, Spec)
import qualified Week01.CreditCardValidatorSpec
import qualified Week02.LogAnalysisSpec
import qualified Week03.GolfSpec
import qualified Week04.SolnSpec
import qualified Week05.CalcSpec
import qualified Week06.FibonacciSpec
import qualified Week07.JoinListSpec
import qualified Week08.PartySpec
import qualified Week10.AParserSpec

spec :: Spec
spec =  do
  Week01.CreditCardValidatorSpec.spec
  Week02.LogAnalysisSpec.spec
  Week03.GolfSpec.spec
  Week04.SolnSpec.spec
  Week05.CalcSpec.spec
  Week06.FibonacciSpec.spec
  Week07.JoinListSpec.spec
  Week08.PartySpec.spec
  Week10.AParserSpec.spec

main :: IO ()
main = hspec spec
