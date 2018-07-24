module Main
  ( main
  ) where

import Test.Hspec (hspec, Spec)
import qualified Week01.CreditCardValidatorSpec

spec :: Spec
spec =  do
  Week01.CreditCardValidatorSpec.spec

main :: IO ()
main = hspec spec
