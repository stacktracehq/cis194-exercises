module Week06.FibonacciSpec
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
import           Week06.Fibonacci               ( fib
                                                , fibs1
                                                , fibs2
                                                , streamToList
                                                , streamRepeat
                                                , streamMap
                                                , streamFromSeed
                                                , nats
                                                , ruler
                                                , fibs3
                                                , fib4
                                                )

spec :: Spec
spec = describe "Week 04" $ do
  fibSpec
  fibs1Spec
  fibs2Spec
  streamToListSpec
  streamRepeatSpec
  streamMapSpec
  streamFromSeedSpec
  natsSpec
  rulerSpec
  fibs3Spec
  fib4Spec

fib0Tofib100 :: [Integer]
fib0Tofib100 =
  [ 0
  , 1
  , 1
  , 2
  , 3
  , 5
  , 8
  , 13
  , 21
  , 34
  , 55
  , 89
  , 144
  , 233
  , 377
  , 610
  , 987
  , 1597
  , 2584
  , 4181
  , 6765
  , 10946
  , 17711
  , 28657
  , 46368
  , 75025
  , 121393
  , 196418
  , 317811
  , 514229
  , 832040
  , 1346269
  , 2178309
  , 3524578
  , 5702887
  , 9227465
  , 14930352
  , 24157817
  , 39088169
  , 63245986
  , 102334155
  , 165580141
  , 267914296
  , 433494437
  , 701408733
  , 1134903170
  , 1836311903
  , 2971215073
  , 4807526976
  , 7778742049
  , 12586269025
  , 20365011074
  , 32951280099
  , 53316291173
  , 86267571272
  , 139583862445
  , 225851433717
  , 365435296162
  , 591286729879
  , 956722026041
  , 1548008755920
  , 2504730781961
  , 4052739537881
  , 6557470319842
  , 10610209857723
  , 17167680177565
  , 27777890035288
  , 44945570212853
  , 72723460248141
  , 117669030460994
  , 190392490709135
  , 308061521170129
  , 498454011879264
  , 806515533049393
  , 1304969544928657
  , 2111485077978050
  , 3416454622906707
  , 5527939700884757
  , 8944394323791464
  , 14472334024676221
  , 23416728348467685
  , 37889062373143906
  , 61305790721611591
  , 99194853094755497
  , 160500643816367088
  , 259695496911122585
  , 420196140727489673
  , 679891637638612258
  , 1100087778366101931
  , 1779979416004714189
  , 2880067194370816120
  , 4660046610375530309
  , 7540113804746346429
  , 12200160415121876738
  , 19740274219868223167
  , 31940434634990099905
  , 51680708854858323072
  , 83621143489848422977
  , 135301852344706746049
  , 218922995834555169026
  , 354224848179261915075
  ]

fibSpec :: Spec
fibSpec = describe "fib" $ it "produces the fibonacci number at index n" $ do
  fib 0 `shouldBe` 0
  fib 1 `shouldBe` 1
  fib 2 `shouldBe` 1
  fib 3 `shouldBe` 2
  fib 4 `shouldBe` 3
  fib 5 `shouldBe` 5
  fib 6 `shouldBe` 8
  fib 7 `shouldBe` 13
  fib 8 `shouldBe` 21
  fib 9 `shouldBe` 34
  fib 10 `shouldBe` 55
  fib 11 `shouldBe` 89
  fib 12 `shouldBe` 144
  fib 13 `shouldBe` 233
  fib 14 `shouldBe` 377

fibs1Spec :: Spec
fibs1Spec =
  describe "fibs1"
    $          it "produces an infinite list of all the fibonacci numbers"
    $          take 15 fibs1
    `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

fibs2Spec :: Spec
fibs2Spec =
  describe "fibs2"
    $ it "produces an infinite list of all the fibonacci numbers in linear time"
    $ take 101 fibs2
    `shouldBe` fib0Tofib100

-- The order of the following might look strange.
-- There are a lot of interlocking dependencies.
-- Hop in GHCI and do what you need to :)

streamToListSpec :: Spec
streamToListSpec =
  describe "streamToList"
    $          it "converts an infinite stream to an infinite list"
    $          take 5 (streamToList (streamFromSeed (+ 1) 0))
    `shouldBe` [0, 1, 2, 3, 4]

streamRepeatSpec :: Spec
streamRepeatSpec =
  describe "streamRepeat"
    $          it
                 "generates a stream containing infinitely many copies of the given element"
    $          take 6 (streamToList (streamRepeat 1))
    `shouldBe` [1, 1, 1, 1, 1, 1]

streamMapSpec :: Spec
streamMapSpec =
  describe "streamMap"
    $          it "applies a function to every element of a Stream"
    $          take 4 (streamToList (streamMap (* 2) (streamFromSeed (+ 1) 0)))
    `shouldBe` [0, 2, 4, 6]

streamFromSeedSpec :: Spec
streamFromSeedSpec =
  describe "streamFromSeed"
    $ it "generates a stream by repeatedly applying the provided function"
    $ take 7 (streamToList (streamFromSeed (+ 3) 1))
    `shouldBe` [1, 4, 7, 10, 13, 16, 19]

natsSpec :: Spec
natsSpec =
  describe "nats"
    $          it "contains the infinite list of natural numbers"
    $          take 10 (streamToList nats)
    `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

rulerSpec :: Spec
rulerSpec =
  describe "ruler"
    $          it "corresponds to the ruler function"
    $          take 16 (streamToList ruler)
    `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

fibs3Spec :: Spec
fibs3Spec =
  describe "fibs3"
    $          it "produces an infinite list of all the fibonacci numbers"
    $          take 101 (streamToList fibs3)
    `shouldBe` fib0Tofib100

fib4Spec :: Spec
fib4Spec = describe "fib4" $ do
  it "fib4 0 == 0" $ fib4 0 `shouldBe` 0
  it "fib4 1 == 1" $ fib4 1 `shouldBe` 1
  it "fib4 5 == 5" $ fib4 5 `shouldBe` 5
  it "fib4 100 == 354224848179261915075"
    $          fib4 100
    `shouldBe` 354224848179261915075
