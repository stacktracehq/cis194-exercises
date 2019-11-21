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
  )
where

import           Week05.ExprT
import           Week05.Parser
import qualified Week05.StackVM                as SVM
import qualified Data.Map                      as M

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit n    ) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

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

-- reify :: ExprT -> ExprT
-- reify = id

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit :: Integer -> Integer
  lit = id
  add :: Integer -> Integer -> Integer
  add = (+)
  mul :: Integer -> Integer -> Integer
  mul = (*)

instance Expr Bool where
  lit n = if n < 1 then False else True
  add = (||)
  mul = (&&)

instance Ord MinMax where
  (<=) :: MinMax -> MinMax -> Bool
  (MinMax n) <= (MinMax m) = n <= m

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (flip mod) 7
  add (Mod7 m) (Mod7 n) = Mod7 (mod (m + n) 7)
  mul (Mod7 m) (Mod7 n) = Mod7 (mod (m * n) 7)

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit :: Integer -> SVM.Program
  lit n = [SVM.PushI n]
  add :: SVM.Program -> SVM.Program -> SVM.Program
  add p1 p2 = p1 ++ p2 ++ [SVM.Add]
  mul :: SVM.Program -> SVM.Program -> SVM.Program
  mul p1 p2 = p1 ++ p2 ++ [SVM.Mul]

compile :: String -> SVM.Program
compile = concat . parseExp lit add mul

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
  var :: String -> (M.Map String Integer -> Maybe Integer)
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit :: Integer -> (M.Map String Integer -> Maybe Integer)
  lit i _ = Just i
  add
    :: (M.Map String Integer -> Maybe Integer)
    -> (M.Map String Integer -> Maybe Integer)
    -> M.Map String Integer
    -> Maybe Integer
---------------------------------------- soln 1

  add f g m = case f m of
    Nothing -> Nothing
    Just x  -> case g m of
      Nothing -> Nothing
      Just y  -> Just (x + y)

---------------------------------------- soln 2

-- add f g m = (+) <$> g m <*> f m

---------------------------------------- soln 3

-- add f g m = do
--   x <- f m
--   y <- g m
--   return (x + y)

  mul
    :: (M.Map String Integer -> Maybe Integer)
    -> (M.Map String Integer -> Maybe Integer)
    -> (M.Map String Integer -> Maybe Integer)
---------------------------------------- soln 1

  mul f g m = case f m of
    Nothing -> Nothing
    Just x  -> case g m of
      Nothing -> Nothing
      Just y  -> Just (x * y)

---------------------------------------- soln 2

-- mul f g m = (*) <$> g m <*> f m

---------------------------------------- soln 3

-- mul f g m = do
--   x <- f m
--   y <- g m
--   return (x * y)

-- withVars
--   :: [(String, Integer)]
--   -> (M.Map String Integer -> Maybe Integer)
--   -> Maybe Integer
-- withVars vs ex = ex $ M.fromList vs

