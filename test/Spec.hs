module Main
  ( main
  ) where

import Test.Hspec (hspec)
import Week01.CreditCardValidatorSpec (creditCardValidatorSpec)

main :: IO ()
main = hspec creditCardValidatorSpec
