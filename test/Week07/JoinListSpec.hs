module Week07.JoinListSpec
  ( spec
  , hspec
  )
where

import           Data.Monoid                    ( Product(..) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Week07.JoinList                ( JoinList(..)
                                                , (!!?)
                                                , (+++)
                                                , indexJ
                                                , dropJ
                                                , takeJ
                                                , joinListToList
                                                , scoreLine
                                                )
import           Week07.Sized
import           Week07.Scrabble
import           Week07.Buffer

spec :: Spec
spec = describe "Week 07" $ do
  appendSpec
  indexJSpec
  dropJSpec
  takeJSpec
  scoreStringSpec
  scoreLineSpec
  joinListBufferSpec

exampleList :: JoinList (Product Int) Char
exampleList = Append
  (Product 210)
  (Append
    (Product 30)
    (Single (Product 5) 'y')
    (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))
  )
  (Single (Product 7) 'h')

exampleListSized :: JoinList (Product Int, Size) Char
exampleListSized = Append
  (Product 210, Size 4)
  (Append
    (Product 30, Size 3)
    (Single (Product 5, Size 1) 'y')
    (Append (Product 6, Size 2)
            (Single (Product 2, Size 1) 'e')
            (Single (Product 3, Size 1) 'a')
    )
  )
  (Single (Product 7, Size 1) 'h')

empty :: JoinList (Product Int, Size) Char
empty = Empty

appendSpec :: Spec
appendSpec =
  describe "+++"
    $          it "concats two JoinLists"
    $          joinListToList (exampleList +++ exampleList)
    `shouldBe` "yeahyeah"

indexJSpec :: Spec
indexJSpec = describe "indexJ" $ do
  it "returns Nothing for positive index out of bounds"
    $          indexJ 100 exampleListSized
    `shouldBe` Nothing
  it "returns Nothing for negative index"
    $          indexJ (-1) exampleListSized
    `shouldBe` Nothing
  it "returns correct value for index in bounds" $ do
    indexJ 0 exampleListSized `shouldBe` 0 !!? joinListToList exampleListSized
    indexJ 1 exampleListSized `shouldBe` 1 !!? joinListToList exampleListSized
    indexJ 2 exampleListSized `shouldBe` 2 !!? joinListToList exampleListSized
    indexJ 3 exampleListSized `shouldBe` 3 !!? joinListToList exampleListSized
    indexJ 8 exampleListSized `shouldBe` 8 !!? joinListToList exampleListSized

dropJSpec :: Spec
dropJSpec = describe "dropJ" $ do
  it "dropping from empty list gives empty list"
    $          dropJ 1 empty
    `shouldBe` empty
  it "dropping more elements than are in list gives empty list"
    $          dropJ 8 exampleListSized
    `shouldBe` empty
  it "joinListToList (dropJ n jl) == drop n (joinListToList jl)" $ do
    joinListToList (dropJ 0 exampleListSized)
      `shouldBe` joinListToList exampleListSized
    joinListToList (dropJ 1 exampleListSized)
      `shouldBe` drop 1 (joinListToList exampleListSized)
    joinListToList (dropJ 2 exampleListSized)
      `shouldBe` drop 2 (joinListToList exampleListSized)
    joinListToList (dropJ 3 exampleListSized)
      `shouldBe` drop 3 (joinListToList exampleListSized)

takeJSpec :: Spec
takeJSpec = describe "takeJ" $ do
  it "taking from empty list gives empty list" $ takeJ 1 empty `shouldBe` empty
  it "taking more than the length of the list gives the whole list"
    $          takeJ 8 exampleListSized
    `shouldBe` exampleListSized
  it "joinListToList (takeJ n jl) == take n (joinListToList jl)" $ do
    joinListToList (takeJ 0 exampleListSized) `shouldBe` []
    joinListToList (takeJ 1 exampleListSized)
      `shouldBe` take 1 (joinListToList exampleListSized)
    joinListToList (takeJ 2 exampleListSized)
      `shouldBe` take 2 (joinListToList exampleListSized)
    joinListToList (takeJ 3 exampleListSized)
      `shouldBe` take 3 (joinListToList exampleListSized)

scoreStringSpec :: Spec
scoreStringSpec = describe "scoreString" $ do
  it "scores empty string as 0" $ scoreString "" `shouldBe` Score 0
  it "scores string of special chars as 0"
    $          scoreString "$#@%^!& *&^{}"
    `shouldBe` Score 0
  it "scores 'cat' as 5" $ scoreString "cat" `shouldBe` Score 5
  it "scores 'dog' as 5" $ scoreString "dog" `shouldBe` Score 5
  it "scores 'quickly' as 25" $ scoreString "quickly" `shouldBe` Score 25
  it "scores 'pizzazz' as 45" $ scoreString "pizzazz" `shouldBe` Score 45

scoreLineSpec :: Spec
scoreLineSpec = describe "scoreLine" $ do
  it "scoreLine '' == Single (Score 0) ''" $ scoreLine "" `shouldBe` Single
    (Score 0)
    ""
  it "scoreLine 'cat' == Single (Score 5) 'cat'"
    $          scoreLine "cat"
    `shouldBe` Single (Score 5) "cat"
  it "scoreLine 'quickly' == Single (Score 25) 'quickly'"
    $          scoreLine "quickly"
    `shouldBe` Single (Score 25) "quickly"
  it "scoreLine 'yay ' +++ scoreLine 'haskell!' is correct join list" $ do
    let result = Append (Score 23)
                        (Single (Score 9) "yay ")
                        (Single (Score 14) "haskell!")
    scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` result

emptyJoinList, l1, l2, l3 :: JoinList (Score, Size) String
emptyJoinList = Empty
l1 = Single (Score 30, Size 1) "cat quickly"
l2 = Single (Score 23, Size 1) "yay haskell!"
l3 = Append (Score 53, 2) l1 l2

l1R, l2R, l3R0, l3R1, l3R :: JoinList (Score, Size) String
replacement :: String
replacement = "dog pizzazz"
l1R = Single (Score 50, Size 1) replacement
l2R = Single (Score 50, Size 1) replacement
l3R0 = Append (Score 73, Size 2) l1R l2
l3R1 = Append (Score 80, Size 2) l1 l2R
l3R = Append (Score 100, Size 2) l1R l2R

joinListBufferSpec :: Spec
joinListBufferSpec =
  describe "instance Buffer (JoinList (Score, Size) String)" $ do
    describe "toString" $ do
      it "empty join list is empty string"
        $          toString emptyJoinList
        `shouldBe` ""
      it "join list with single element should be the single value"
        $          toString l1
        `shouldBe` "cat quickly"
      it
          "join list with 2 elements should be the string concatenation of the values"
        $          toString l3
        `shouldBe` "cat quicklyyay haskell!"
    describe "fromString" $ do
      it "empty string gives empty list"
        $          fromString ""
        `shouldBe` emptyJoinList
      it "string without new line => correctly scored single element list"
        $          fromString "cat quickly"
        `shouldBe` l1
      it "string with newline => correctly scored 2-elem list"
        $          fromString "cat quickly\nyay haskell!"
        `shouldBe` l3
    describe "line" $ do
      it "returns Nothing for index out of bounds" $ do
        line 100 l3 `shouldBe` Nothing
        line (-1) l3 `shouldBe` Nothing
      it "returns element at index" $ do
        line 0 l1 `shouldBe` Just "cat quickly"
        line 0 l2 `shouldBe` Just "yay haskell!"
        line 0 l3 `shouldBe` Just "cat quickly"
        line 1 l3 `shouldBe` Just "yay haskell!"
    describe "replaceLine" $ do
      it "replacing line in empty list returns empty list"
        $          replaceLine 4 replacement emptyJoinList
        `shouldBe` emptyJoinList
      it "replaceing line with index out of bounds returns input list unchanged"
        $          replaceLine 100 replacement l3
        `shouldBe` l3
      it "replaces line and score and size remain correct" $ do
        replaceLine 0 replacement l1 `shouldBe` l1R
        replaceLine 0 replacement l2 `shouldBe` l2R
        replaceLine 0 replacement l3 `shouldBe` l3R0
        replaceLine 1 replacement l3 `shouldBe` l3R1
        replaceLine 1 replacement (replaceLine 0 replacement l3) `shouldBe` l3R
    describe "numLines" $ do
      it "returns 0 for empty list" $ numLines emptyJoinList `shouldBe` 0
      it "returns 1 for singleon list" $ numLines l1 `shouldBe` 1
      it "returns 2 for 2-elem list" $ numLines l3 `shouldBe` 2
    describe "value" $ it "retruns correct score value" $ do
      value emptyJoinList `shouldBe` 0
      value l1 `shouldBe` 30
      value l2 `shouldBe` 23
      value l3 `shouldBe` 53
