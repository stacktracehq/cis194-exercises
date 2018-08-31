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
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)
import qualified Week05.StackVM as SVM
import qualified Data.Map as M

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

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
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit a = [SVM.PushI a]
  add a b = a ++ b ++ [SVM.Add]
  mul a b = a ++ b ++ [SVM.Mul]

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
  var = flip (M.!?)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 ((<*>) . fmap (+))
  mul = liftA2 ((<*>) . fmap (*))
