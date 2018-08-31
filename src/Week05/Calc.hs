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
  ) where

import Week05.ExprT
import Week05.Parser
import qualified Week05.StackVM as SVM
import qualified Data.Map as M
import Data.Maybe

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp Lit Add Mul

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

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
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
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit x = [ SVM.PushI x ]
  mul x y = x ++ y ++ [ SVM.Mul ]
  add x y = x ++ y ++ [ SVM.Add ]

compile :: String -> SVM.Program
compile = fromMaybe [] . parseExp lit add mul

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
  var x = (M.!? x)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add fx fy m = (+) <$> fx m <*> fy m
  mul fx fy m  = (*) <$> fx m <*> fy m
