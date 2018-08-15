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

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval = error "Week05.Calc#eval not implemented"

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = error "Week05.Calc#evalStr not implemented"

--------------------------------------------------- Exercise 3

-- This is our type class :)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- now write an instance for our ExprT type
instance Expr ExprT where
  lit = error "Week05.Calc#lit not implemented for ExprT"
  add = error "Week05.Calc#add not implemented for ExprT"
  mul = error "Week05.Calc#mul not implemented for ExprT"

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = error "Week05.Calc#lit not implemented for Integer"
  add = error "Week05.Calc#lit not implemented for Integer"
  mul = error "Week05.Calc#lit not implemented for Integer"

instance Expr Bool where
  lit = error "Week05.Calc#lit not implemented for Bool"
  add = error "Week05.Calc#add not implemented for Bool"
  mul = error "Week05.Calc#mul not implemented for Bool"

instance Expr MinMax where
  lit = error "Week05.Calc#lit not implemented for MinMax"
  add = error "Week05.Calc#add not implemented for MinMax"
  mul = error "Week05.Calc#mul not implemented for MinMax"

instance Expr Mod7 where
  lit = error "Week05.Calc#lit not implemented for Mod7"
  add = error "Week05.Calc#add not implemented for Mod7"
  mul = error "Week05.Calc#mul not implemented for Mod7"

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit = error "Week05.Calc#mul not implemented for Program"
  add = error "Week05.Calc#mul not implemented for Program"
  mul = error "Week05.Calc#mul not implemented for Program"

compile :: String -> SVM.Program
compile = error "Week05.Calc#compile not implemented"

--------------------------------------------------- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = LitWithVar Integer
  | AddWithVar VarExprT VarExprT
  | MulWithVar VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = error "Week05.Calc#lit not implemented for VarExprT"
  add = error "Week05.Calc#add not implemented for VarExprT"
  mul = error "Week05.Calc#mul not implemented for VarExprT"

instance HasVars VarExprT where
  var = error "Week05.Calc#var not implemented for VarExprT"

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = error "Week05.Calc#var not implemented for (M.Map String Integer -> Maybe Integer)"

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = error "Week05.Calc#lit not implemented for (M.Map String Integer -> Maybe Integer)"
  add = error "Week05.Calc#add not implemented for (M.Map String Integer -> Maybe Integer)"
  mul = error "Week05.Calc#mul not implemented for (M.Map String Integer -> Maybe Integer)"
