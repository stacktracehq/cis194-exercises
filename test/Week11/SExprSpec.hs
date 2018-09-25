module Week11.SExprSpec
  ( spec
  , hspec
  ) where

import Data.Char (isAlpha, isAlphaNum, isLower, isSpace, isUpper, ord, toUpper)
import Data.List (dropWhileEnd)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (MonadGen(..), (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)
import Week11.AParser (runParser, satisfy)
import Week11.SExpr
  ( Atom(..)
  , SExpr(..)
  , ident
  , oneOrMore
  , parseSExpr
  , spaces
  , zeroOrMore
  )

spec :: Spec
spec =
  describe "Week 11" $ do
    zeroOrMoreSpec
    oneOrMoreSpec
    spacesSpec
    identSpec
    parseSExprSpec

type Bounds = (Int, Int)

stringInBounds :: MonadGen m => m Char -> Bounds -> m String
stringInBounds mc (minLength, maxLength) =
  Gen.string (Range.linear minLength maxLength) mc

alphaNumString :: MonadGen m => Bounds -> m String
alphaNumString = stringInBounds Gen.alphaNum

alphaString :: MonadGen m => Bounds -> m String
alphaString = stringInBounds Gen.alpha

upperCaseString :: MonadGen m => Bounds -> m String
upperCaseString = stringInBounds Gen.upper

lowerCaseString :: MonadGen m => Bounds -> m String
lowerCaseString = stringInBounds Gen.lower

digitString :: MonadGen m => Bounds -> m String
digitString = stringInBounds Gen.digit

zeroOrMoreSpec :: Spec
zeroOrMoreSpec =
  describe "zeroOrMore" $
  it "parses zero or more occurences from start" $
  require $
  property $ do
    uppers <- forAll $ upperCaseString (0, 100)
    lowers <- forAll $ lowerCaseString (0, 100)
    let input = uppers <> lowers
    runParser (zeroOrMore (satisfy isUpper)) input === Just (uppers, lowers)

oneOrMoreSpec :: Spec
oneOrMoreSpec =
  describe "oneOrMore" $ do
    it "parses one or more occurences from start" $
      require $
      property $ do
        uppers <- forAll $ upperCaseString (1, 100)
        lowers <- forAll $ lowerCaseString (0, 100)
        let input = uppers <> lowers
        runParser (oneOrMore (satisfy isUpper)) input === Just (uppers, lowers)
    it "fails when no matches at start" $
      require $
      property $ do
        digits <- forAll $ digitString (1, 100)
        uppers <- forAll $ upperCaseString (0, 100)
        let input = digits <> uppers
        runParser (oneOrMore (satisfy isUpper)) input === Nothing

spacesSpec :: Spec
spacesSpec =
  describe "spaces" $
  it "parses zero or more spaces from the start" $
    require $
    property $ do
      numSpaces <- forAll $ Gen.int (Range.linear 0 100)
      rest <- forAll $ alphaNumString (0, 100)
      let spaceStr = replicate numSpaces ' '
      let input = spaceStr <> rest
      runParser spaces input === Just (spaceStr, rest)

identSpec :: Spec
identSpec =
  describe "ident" $ do
    it "successfully parses identifier from strings starting with alphas" $
      require $
      property $ do
        let seperators = [' ', '!', '$', '^', '\t', '\n']
        identifer <- forAll $ alphaString (1, 100)
        sep <- forAll $ Gen.choice $ Gen.constant <$> seperators
        rest <- forAll $ alphaNumString (1, 100)
        let input = identifer <> [sep] <> rest
        runParser ident input === Just (identifer, [sep] <> rest)
    it "fails when first char is not an alpha" $
      require $
      property $ do
        start <- forAll $ digitString (1, 50)
        rest <- forAll $ alphaString (0, 100)
        let input = start <> rest
        runParser ident input === Nothing

genIdent :: MonadGen m => m String
genIdent = do
  firstChar <- alphaString (1, 1)
  rest <- alphaNumString (0, 100)
  return (firstChar <> rest)

genAtom :: MonadGen m => m String
genAtom = Gen.choice [digitString (1, 10), genIdent]

surroundWithZeroOrMore :: MonadGen m => Char -> m String -> m String
surroundWithZeroOrMore c ms = do
  numLeading <- Gen.int (Range.linear 0 100)
  numTrailing <- Gen.int (Range.linear 0 100)
  s <- ms
  return (replicate numLeading c <> s <> replicate numTrailing c)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

atom :: String -> Atom
atom input =
  case trim input of
    str@(s:_) ->
      if isAlpha s
        then I str
        else N (read str)
    _ -> error "invalid atom format"

ai :: String -> SExpr
ai = A . I

an :: Integer -> SExpr
an = A . N

sexprs :: [String] -> [SExpr]
sexprs = (<$>) (A . atom)

parseSExprSpec :: Spec
parseSExprSpec =
  describe "parseSExpr" $ do
    context "property based..." $ do
      it "parses single atoms successfully" $
        require $
        property $ do
          input <- forAll $ surroundWithZeroOrMore ' ' genAtom
          runParser parseSExpr input === Just ((A . atom) input, "")
      it "parses single level of atoms inside parens" $
        require $
        property $ do
          atoms <- forAll $ Gen.list (Range.linear 1 20) genAtom
          let input = "(" <> unwords atoms <> ")"
          let expected = Comb (sexprs atoms)
          runParser parseSExpr input === Just (expected, "")
      it "parses nested SExprs successfully" $
        require $
        property $ do
          atoms1 <- forAll $ Gen.list (Range.linear 1 20) genAtom
          atoms2 <-
            forAll $
            Gen.list (Range.linear 1 20) (surroundWithZeroOrMore ' ' genAtom)
          atoms3 <- forAll $ Gen.list (Range.linear 1 20) genAtom
          let input =
                "((" <> unwords atoms1 <> ") " <> unwords atoms2 <> "(" <>
                unwords atoms3 <>
                "))"
          let expected =
                Comb
                  ([Comb (sexprs atoms1)] ++
                   sexprs atoms2 ++ [Comb (sexprs atoms3)])
          runParser parseSExpr input === Just (expected, "")
    context "manual..." $ do
      it "parsing unbalanced parens fails" $ do
        let input = "((((one)))"
        runParser parseSExpr input `shouldBe` Nothing
      it "parsing '(bar (foo) 3 5 874)' succeeds" $ do
        let input = "(bar (foo) 3 5 874)"
        let expected = Comb [ai "bar", Comb [ai "foo"], an 3, an 5, an 874]
        runParser parseSExpr input `shouldBe` Just (expected, "")
      it "parsing '(mul (2 (add (42 56)))' succeeds" $ do
        let input = "(mul (2 (add (42 56))))"
        let expected =
              Comb [ai "mul", Comb [an 2, Comb [ai "add", Comb [an 42, an 56]]]]
        runParser parseSExpr input `shouldBe` Just (expected, "")
      it "parsing '( lots of ( spaces in ) this ( one ) )' succeeds" $ do
        let input = "( lots of ( spaces in ) this ( one ) )"
        let expected =
              Comb
                [ ai "lots"
                , ai "of"
                , Comb [ai "spaces", ai "in"]
                , ai "this"
                , Comb [ai "one"]
                ]
        runParser parseSExpr input `shouldBe` Just (expected, "")
