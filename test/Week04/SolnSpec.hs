module Week04.SolnSpec
  ( spec
  , hspec
  ) where

import Test.Hspec (Spec, describe, hspec, it, context, shouldBe, shouldSatisfy)
import Data.List ((\\))
import Week04.Soln
  ( fun1
  , fun1'
  , fun2
  , fun2'
  , foldTree
  , Tree(..)
  , xor
  , map'
  , myFoldl
  , sieveSundaram
  )

spec :: Spec
spec =
  describe "Week 04" $ do
    fun1'Spec
    fun2'Spec
    foldTreeSpec
    xorSpec
    map'Spec
    myFoldlSpec
    sieveSundaramSpec

-- a lot of these specs would be prime candidates for property based testing,
-- but I don't really know how....

fun1'Spec :: Spec
fun1'Spec =
  describe "fun1', the 'wholemeal' version of fun1" $
    it "fun1' xs == fun1 xs" $ do
      fun1 [] `shouldBe` fun1' []
      fun1 [4] `shouldBe` fun1' [4]
      fun1 [2,4,6,8] `shouldBe` fun1' [2,4,6,8]
      fun1 [1,3,5,7] `shouldBe` fun1' [1,3,5,7]
      fun1 [1..20] `shouldBe` fun1' [1..20]
      fun1 [24, 88, 66] `shouldBe` fun1' [24, 88, 66]

fun2'Spec :: Spec
fun2'Spec =
  describe "fun2', the 'wholemeal' version of fun2" $
    it "fun2' x == fun2 x" $ do
      fun2 3 `shouldBe` fun2' 3
      fun2 24 `shouldBe` fun2' 24
      fun2 12 `shouldBe` fun2' 12
      fun2 123 `shouldBe` fun2' 123

foldTreeSpec :: Spec
foldTreeSpec =
  describe "foldTree" $ do
    let abcResult = foldTree "ABC"
    let abcdResult = foldTree "ABCD"
    let abcdefghijResult = foldTree "ABCDEFGHIJ"
    let oneTo100Result = foldTree [1..100]

    it "produces a single leaf for an empty list" $
      foldTree ([] :: String) `shouldBe` Leaf
    it "produces a tree of height 0 for a singleton list" $
      foldTree ['a'] `shouldBe` Node 0 Leaf 'a' Leaf
    context "produces trees of the correct height" $ do
      it "height of foldTree 'ABC' == 1" $
        treeHeight abcResult `shouldBe` 1
      it "height of foldTree 'ABCD' == 2" $
        treeHeight abcdResult `shouldBe` 2
      it "height of foldTree 'ABCDEFGHIJ' == 3" $
        treeHeight abcdefghijResult `shouldBe` 3
      it "height of foldTree [1..100] == 6" $
        treeHeight oneTo100Result `shouldBe` 6
    context "produces balanced trees" $ do
      it "foldTree 'ABC' is balanced" $
        abcResult `shouldSatisfy` isBalanced
      it "foldTree 'ABCD' is balanced" $
        abcdResult `shouldSatisfy` isBalanced
      it "foldTree 'ABCDEFGHIJ' is balanced" $
        abcdefghijResult `shouldSatisfy` isBalanced
      it "foldTree [1..100] is balanced" $
        oneTo100Result `shouldSatisfy` isBalanced
    context "produces trees that contain all the input values" $ do
      it "foldTree 'ABC' contains all inputs" $
        unfoldTree abcResult `shouldSatisfy` containsAllInputs "ABC"
      it "foldTree 'ABCD' contains all inputs" $
        unfoldTree abcdResult `shouldSatisfy` containsAllInputs "ABCD"
      it "foldTree 'ABCDEFGHIJ' contains all inputs" $
        unfoldTree abcdefghijResult `shouldSatisfy` containsAllInputs "ABCDEFGHIJ"
      it "foldTree [1..100] contains all inputs" $
        unfoldTree oneTo100Result `shouldSatisfy` containsAllInputs [1..100]
  where
    treeHeight Leaf = -1
    treeHeight (Node h _ _ _ ) = h
    balancedAtNode lt rt = abs (treeHeight lt - treeHeight rt) <= 1
    isBalanced Leaf = True
    isBalanced (Node _ lt _ rt) = balancedAtNode lt rt && isBalanced lt && isBalanced rt
    unfoldTree Leaf = [] :: [a]
    unfoldTree (Node _ lt x rt) = x : (unfoldTree lt ++ unfoldTree rt)
    containsAllInputs xs ys = null (xs \\ ys) && null (ys \\ xs)

xorSpec :: Spec
xorSpec =
  describe "xor" $ do
    it "xor [] == False" $
      xor [] `shouldBe` False
    it "xor [False] == False" $
      xor [False] `shouldBe` False
    it "xor [True] == True" $
      xor [True] `shouldBe` True
    it "xor [True, True, True] == True" $
      xor [True, True, True] `shouldBe` True
    it "xor [True, True] == False" $
      xor [True, True] `shouldBe` False
    it "xor [False, True, False] == True" $
      xor [False, True, False] `shouldBe` True
    it "xor [False, True, False, False, True] == False" $
      xor [False, True, False, False, True] `shouldBe` False

-- another good candidate for prop based testing?
map'Spec :: Spec
map'Spec =
  describe "map'" $ do
    it "map' f [] == map f []" $
      map' (+1) [] `shouldBe` map (+1) []
    it "map' f [x] == map f [x]" $
      map' (*2) [2] `shouldBe` map (*2) [2]
    it "map' f [x,y,z] == map f [x,y,z]" $ do
      map' show [4,5,6] `shouldBe` map show [4,5,6]
      map' (*2) [4,5,6] `shouldBe` map (*2) [4,5,6]

-- and again?
myFoldlSpec :: Spec
myFoldlSpec =
  describe "myFoldlSpec" $ do
    it "myFoldl f b [] == foldl f b []" $
      myFoldl (+) 0 [] `shouldBe` foldl (+) 0 []
    it "myFoldl f b [x] == foldl f b [x]" $
      myFoldl (*) 3 [2] `shouldBe` foldl (*) 3 [2]
    it "myFoldl f b [x,y,z] == foldl f b [x,y,z]" $ do
      myFoldl (&&) True [False, True, False] `shouldBe` foldl (&&) True [False, True, False]
      myFoldl (&&) True [True, True, True] `shouldBe` foldl (&&) True [True, True, True]

sieveSundaramSpec :: Spec
sieveSundaramSpec =
  describe "sieveSundaram" $ do
    it "sieveSundaram 1 == [3]" $
      sieveSundaram 1 `shouldBe` [3]
    it "sieveSundaram 2 == [3,5]" $
      sieveSundaram 2 `shouldBe` [3,5]
    it "sieveSundaram 2 == [3,5]" $
      sieveSundaram 2 `shouldBe` [3,5]
    it "sieveSundaram 5 == [3,5,7,11]" $
      sieveSundaram 5 `shouldBe` [3,5,7,11]
    it "sieveSundaram 10 == [3,5,7,11,13,17,19]" $
      sieveSundaram 10 `shouldBe` [3,5,7,11,13,17,19]
    it "sieveSundaram 15 == [3,5,7,11,13,17,19,23,29,31]" $
      sieveSundaram 15 `shouldBe` [3,5,7,11,13,17,19,23,29,31]
