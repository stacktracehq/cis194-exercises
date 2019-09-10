module Week01.TowersOfHanoiSpec
  ( spec
  , hspec
  )
where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Week01.TowersOfHanoi           ( hanoi )

spec :: Spec
spec = describe "TowersOfHanoi" hanoiSpec

hanoiSpec :: Spec
hanoiSpec =
  describe "hanoi"
    $ it "lists the moves needed to move _n_ disks from peg a to peg b"
    $ do
        hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
        hanoi 3 "a" "b" "c"
          `shouldBe` [ ("a", "b")
                     , ("a", "c")
                     , ("b", "c")
                     , ("a", "b")
                     , ("c", "a")
                     , ("c", "b")
                     , ("a", "b")
                     ]
