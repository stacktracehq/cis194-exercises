{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Week05.Calc
  ( eval
  , evalStr
  , Expr(..)
  , MinMax(..)
  , Mod7(..)
  , compile
  , HasVars(..)
  , VarExprT(..)
  )
where

import           Week05.ExprT
import           Week05.Parser
import qualified Week05.StackVM                as SVM
import qualified Data.Map                      as M
import           Data.Maybe
import           Control.Applicative (liftA2)

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit n  ) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--------------------------------------------------- Exercise 3

-- This is our type class :)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- now write an instance for our ExprT type
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = lit $ max l r
  mul (MinMax l) (MinMax r) = lit $ min l r

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 l) (Mod7 r) = lit $ l + r
  mul (Mod7 l) (Mod7 r) = lit $ l * r

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit n = [SVM.PushI n]
  add l r = l ++ r ++ [SVM.Add]
  mul l r = l ++ r ++ [SVM.Mul]

compile :: String -> SVM.Program
compile s = fromMaybe [] $ parseProgram s
 where
  parseProgram :: String -> Maybe SVM.Program
  parseProgram = parseExp lit add mul

--------------------------------------------------- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = LitWithVar Integer
  | AddWithVar VarExprT VarExprT
  | MulWithVar VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = LitWithVar
  add = AddWithVar
  mul = MulWithVar

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add l r m = liftA2 (+) (l m) (r m)
  mul l r m = liftA2 (*) (l m) (r m)
  -- add l r m = (+) <$> (l m) <*> (r m)
  -- mul l r m = (*) <$> (l m) <*> (r m)
