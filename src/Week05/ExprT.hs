-- CIS 194 Homework 5
module Week05.ExprT
  ( ExprT(..)
  ) where

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
