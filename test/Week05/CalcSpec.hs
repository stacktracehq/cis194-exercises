module Week05.CalcSpec
  ( spec
  , hspec
  )
where

import qualified Data.Map                      as M
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , context
                                                , shouldBe
                                                )
import           Week05.ExprT
import           Week05.Parser
import qualified Week05.StackVM                as SVM
import           Week05.Calc                    ( eval
                                                , evalStr
                                                , Expr(..)
                                                , MinMax(..)
                                                , Mod7(..)
                                                , compile
                                                , HasVars(..)
                                                , VarExprT(..)
                                                )

spec :: Spec
spec = describe "Week 05" $ do
  exerciseOneSpec
  exerciseTwoSpec
  exerciseThreeSpec
  exerciseFourSpec
  exerciseFiveSpec
  exerciseSixSpec

exerciseOneSpec :: Spec
exerciseOneSpec = describe "Exercise 1" $ context "eval" $ do
  it "eval (Lit 3) shouldBe 3" $ eval (Lit 3) `shouldBe` 3
  it "eval (Add (Lit 2) (Lit 5)) shouldBe 7"
    $          eval (Add (Lit 2) (Lit 5))
    `shouldBe` 7
  it "eval (Mul (Lit 2) (Lit 7)) shouldBe 14"
    $          eval (Mul (Lit 2) (Lit 7))
    `shouldBe` 14
  it "eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) shouldBe 20"
    $          eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    `shouldBe` 20
  it "eval (Add (Mul (Lit 5) (Lit 2)) (Lit 4)) shouldBe 14"
    $          eval (Add (Mul (Lit 5) (Lit 2)) (Lit 4))
    `shouldBe` 14
  it "eval (Add (Add (Lit 5) (Lit 2)) (Lit 4)) shouldBe 11"
    $          eval (Add (Add (Lit 5) (Lit 2)) (Lit 4))
    `shouldBe` 11
  it "eval (Add (Add (Mul (Lit 3) (Lit 3)) (Lit 2)) (Lit 4)) shouldBe 15"
    $          eval (Add (Add (Mul (Lit 3) (Lit 3)) (Lit 2)) (Lit 4))
    `shouldBe` 15

exerciseTwoSpec :: Spec
exerciseTwoSpec = describe "Exercise 2" $ context "evalStr" $ do
  it "evalStr \"3\" shouldBe Just 3" $ evalStr "3" `shouldBe` Just 3
  it "evalStr \"2 + 5\" shouldBe Just 7" $ evalStr "2 + 5" `shouldBe` Just 7
  it "evalStr \"2 * 7\" shouldBe Just 14" $ evalStr "2 * 7" `shouldBe` Just 14
  it "evalStr \"(2 * 6) + 5\" shouldBe Just 17"
    $          evalStr "(2 * 6) + 5"
    `shouldBe` Just 17
  it "evalStr \"(3 + 4) * 2\" shouldBe Just 14"
    $          evalStr "(3 + 4) * 2"
    `shouldBe` Just 14
  it "evalStr \"(3 + 4 + 5) * (2 * 3)\" shouldBe Just 72"
    $          evalStr "(3 + 4 + 5) * (2 * 3)"
    `shouldBe` Just 72
  it "evalStr \"3 + \" shouldBe Nothing" $ evalStr "3 + " `shouldBe` Nothing
  it "evalStr \"6 * \" shouldBe Nothing" $ evalStr "6 * " `shouldBe` Nothing
  it "evalStr \"6 + (8 / 2)\" shouldBe Nothing"
    $          evalStr "6 + (8 / 2)"
    `shouldBe` Nothing

exerciseThreeSpec :: Spec
exerciseThreeSpec =
  describe "Exercise 3"
    $ context "instance Expr ExprT"
    $ it "lit, add, and mul should construct correct ExprT values"
    $ do
        lit 2 `shouldBe` Lit 2
        add (lit 3) (lit 5) `shouldBe` Add (Lit 3) (Lit 5)
        mul (lit 3) (lit 5) `shouldBe` Mul (Lit 3) (Lit 5)
        mul (add (lit 2) (lit 3)) (lit 4)
          `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

reifyI :: Maybe Integer -> Maybe Integer
reifyI = id

reifyB :: Maybe Bool -> Maybe Bool
reifyB = id

reifyMinMax :: Maybe MinMax -> Maybe MinMax
reifyMinMax = id

reifyMod7 :: Maybe Mod7 -> Maybe Mod7
reifyMod7 = id

reifyProgram :: Maybe SVM.Program -> Maybe SVM.Program
reifyProgram = id

evalWithInstance :: Expr a => (Maybe a -> Maybe a) -> String -> Maybe a
evalWithInstance r s = r $ parseExp lit add mul s

exerciseFourSpec :: Spec
exerciseFourSpec = describe "Exercise 4" $ do
  context "instance Expr Integer" $ do
    let evalIntegerExp = evalWithInstance reifyI
    it "(3 * -4) + 5 shouldBe Just (-7)"
      $          evalIntegerExp "(3 * -4) + 5"
      `shouldBe` Just (-7)
    it "(3 + 2) * 4 shouldBe Just 20"
      $          evalIntegerExp "(3 + 2) * 4"
      `shouldBe` Just 20
    it "3 + 6 + 8 shouldBe Just 17" $ evalIntegerExp "3 + 6 + 8" `shouldBe` Just
      17
    it "2 * 3 * 4 shouldBe Just 24" $ evalIntegerExp "2 * 3 * 4" `shouldBe` Just
      24

  context "instance Expr Bool" $ do
    let evalBoolExp = evalWithInstance reifyB
    it "(1 * 0) + 1 shouldBe Just True"
      $          evalBoolExp "(1 * 0) + 1"
      `shouldBe` Just True
    it "(1 + 0) * 1 shouldBe Just True"
      $          evalBoolExp "(1 + 0) * 1"
      `shouldBe` Just True
    it "(0 + 0) * 1 shouldBe Just False"
      $          evalBoolExp "(0 + 0) * 1"
      `shouldBe` Just False
    it "(1 * 1) * 0 shouldBe Just False"
      $          evalBoolExp "(1 * 1) * 0"
      `shouldBe` Just False
    it "1 * 1 * 1 * 0 shouldBe Just False"
      $          evalBoolExp "1 * 1 * 1 * 0"
      `shouldBe` Just False
    it "0 + 0 + 0 + 1 shouldBe Just True"
      $          evalBoolExp "0 + 0 + 0 + 1"
      `shouldBe` Just True

  context "instance Expr MinMax" $ do
    let evalMinMaxExp = evalWithInstance reifyMinMax
    it "1 shouldBe Just (MinMax 1)" $ evalMinMaxExp "1" `shouldBe` Just
      (MinMax 1)
    it "1 + 4 shouldBe Just (MinMax 4)" $ evalMinMaxExp "1 + 4" `shouldBe` Just
      (MinMax 4)
    it "2 * 5 shouldBe Just (MinMax 2)" $ evalMinMaxExp "2 * 5" `shouldBe` Just
      (MinMax 2)
    it "4 * (5 + 2) shouldBe Just (MinMax 4)"
      $          evalMinMaxExp "4 * 5"
      `shouldBe` Just (MinMax 4)

  context "instance Expr Mod7" $ do
    let evalMod7Exp = evalWithInstance reifyMod7
    it "1 shouldBe Just (Mod7 1)" $ evalMod7Exp "1" `shouldBe` Just (Mod7 1)
    it "7 shouldBe Just (Mod7 0)" $ evalMod7Exp "7" `shouldBe` Just (Mod7 0)
    it "9 shouldBe Just (Mod7 2)" $ evalMod7Exp "9" `shouldBe` Just (Mod7 2)
    it "4 * 3 shouldBe Just (Mod7 5)" $ evalMod7Exp "4 * 3" `shouldBe` Just
      (Mod7 5)
    it "4 + 2 shouldBe Just (Mod7 6)" $ evalMod7Exp "4 + 2" `shouldBe` Just
      (Mod7 6)
    it "(4 + 6) * (2 + 3) shouldBe Just (Mod7 1)"
      $          evalMod7Exp "(4 + 6) * (2 + 3)"
      `shouldBe` Just (Mod7 1)

exerciseFiveSpec :: Spec
exerciseFiveSpec = describe "Exercise 5" $ do
  context "instance Expr Program" $ do
    let evalProgramExp = evalWithInstance reifyProgram
    it "1 shouldBe Just [PushI 1]" $ evalProgramExp "1" `shouldBe` Just
      [SVM.PushI 1]
    it "1 + 2 shouldBe Just [PushI 1, PushI 2, Add]"
      $          evalProgramExp "1 + 2"
      `shouldBe` Just [SVM.PushI 1, SVM.PushI 2, SVM.Add]
    it "2 * 3 shouldBe Just [PushI 2, PushI 3, Mul]"
      $          evalProgramExp "2 * 3"
      `shouldBe` Just [SVM.PushI 2, SVM.PushI 3, SVM.Mul]
    it
        "(2 * 3) + (4 * 5) shouldBe Just [PushI 2, PushI 3, Mul, PushI 4, PushI 5, Mul, Add]"
      $          evalProgramExp "(2 * 3) + (4 * 5)"
      `shouldBe` Just
                   [ SVM.PushI 2
                   , SVM.PushI 3
                   , SVM.Mul
                   , SVM.PushI 4
                   , SVM.PushI 5
                   , SVM.Mul
                   , SVM.Add
                   ]

  context "compile" $ do
    it "compile \"1\" shouldBe [PushI 1]" $ compile "1" `shouldBe` [SVM.PushI 1]
    it "compile \"1 + 4\" shouldBe [SVM.PushI 1, SVM.PushI 4, Add]"
      $          compile "1 + 4"
      `shouldBe` [SVM.PushI 1, SVM.PushI 4, SVM.Add]
    it "compile \"5 * 3\" shouldBe [SVM.PushI 5, SVM.PushI 3, Mul]"
      $          compile "5 * 3"
      `shouldBe` [SVM.PushI 5, SVM.PushI 3, SVM.Mul]
    it
        "compile \"(2 * 3) + (4 * 5)\" shouldBe Just [PushI 2, PushI 3, Mul, PushI 4, PushI 5, Mul, Add]"
      $          compile "(2 * 3) + (4 * 5)"
      `shouldBe` [ SVM.PushI 2
                 , SVM.PushI 3
                 , SVM.Mul
                 , SVM.PushI 4
                 , SVM.PushI 5
                 , SVM.Mul
                 , SVM.Add
                 ]
    it "compile \"5 * \" shouldBe []" $ compile "5 * " `shouldBe` []

withVars
  :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs ex = ex $ M.fromList vs

exerciseSixSpec :: Spec
exerciseSixSpec = describe "Exercise 6" $ do
  context "instance Expr VarExprT" $ do
    it
        "add (lit 3) (var \"x\") `shouldBe` AddWithVar (LitWithVar 3) (Var \"x\")"
      $          add (lit 3) (var "x")
      `shouldBe` AddWithVar (LitWithVar 3) (Var "x")
    it
        "mul (lit 7) (var \"7\") `shouldBe` MulWithVar (LitWithVar 7) (Var \"y\")"
      $          mul (lit 7) (var "y")
      `shouldBe` MulWithVar (LitWithVar 7) (Var "y")

  context "calculate expressions with vars" $ do
    it "withVars [(\"x\", 6)] $ add (lit 3) (var \"x\") `shouldBe` Just 9"
      $          withVars [("x", 6)] (add (lit 3) (var "x"))
      `shouldBe` Just 9
    it "withVars [(\"x\", 6)] $ add (lit 3) (var \"y\")"
      $          withVars [("x", 6)] (add (lit 3) (var "y"))
      `shouldBe` Nothing
    it
        "withVars [(\"x\", 6), (\"y\", 3)] $ mul (var \"x\") (add (var \"y\") (var \"x\"))"
      $ withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x")))
      `shouldBe` Just 54
