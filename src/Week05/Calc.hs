{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

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

import Data.Maybe (fromMaybe)
import Week05.ExprT
import Week05.Parser
import qualified Data.Map as M
import qualified Week05.StackVM as SVM

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

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
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit = pure . SVM.PushI
  add as bs = as ++ bs ++ [SVM.Add]
  mul as bs = as ++ bs ++ [SVM.Mul]

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
  lit :: Integer -> VarExprT
  lit = LitWithVar

  add :: VarExprT -> VarExprT -> VarExprT
  add = AddWithVar

  mul :: VarExprT -> VarExprT -> VarExprT
  mul = MulWithVar

instance HasVars VarExprT where
  var :: String -> VarExprT
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var :: String -> M.Map String Integer -> Maybe Integer
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit :: Integer -> M.Map String Integer -> Maybe Integer
  lit = const . Just

  add ::
    (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) ->
    M.Map String Integer -> Maybe Integer
  add f g m = (+) <$> f m <*> g m

  mul ::
    (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) ->
    M.Map String Integer -> Maybe Integer
  mul f g m = (*) <$> f m <*> g m
