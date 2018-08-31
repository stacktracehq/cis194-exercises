{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Week07.JoinList
  ( JoinList(..)
  , joinListToList
  , (!!?)
  , (+++)
  , indexJ
  , dropJ
  , takeJ
  , scoreLine
  , tag
  , main
  ) where

import Week07.Buffer (Buffer(..))
import Week07.Editor (editor, runEditor)
import Week07.Scrabble (Score(..), scoreString)
import Week07.Sized (Sized(..), Size(..), getSize)
import Data.Monoid

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ a) = [a]
joinListToList (Append _ l r) = joinListToList l ++ joinListToList r

(!!?) :: Int -> [a] -> Maybe a
(!!?) n = lookup n . zip [0 ..]

--------------------------- Exercise 1

-- Suggestion (no tests):
-- Pulls the monoidal value out of the root of the JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

--------------------------- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ _) | i > 0 = Nothing
indexJ i (Single _ _) | i > 0 = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ x y) 
  | Size i < size (tag x) = indexJ i x
  | otherwise = indexJ (i - getSize (size (tag x))) y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 x = x
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append m _ _) | i >= getSize (size m) = Empty
dropJ i (Append _ x y)
  | Size i < size (tag x) = dropJ i x +++ y
  | otherwise = dropJ (i - getSize (size (tag x))) y

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ i x | i >= getSize (size (tag x)) = x
takeJ _ x@(Single _ _) = x
takeJ i (Append _ x y)
  | Size i < size (tag x) = takeJ i x
  | otherwise = x +++ takeJ (i - getSize (size (tag x))) y

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = error "Week07.JoinList#toString not implemented for Buffer (JoinList (Score, Size) String)"

  fromString :: String -> JoinList (Score, Size) String
  fromString = error "Week07.JoinList#fromString not implemented for Buffer (JoinList (Score, Size) String)"

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = error "Week07.JoinList#line not implemented for Buffer (JoinList (Score, Size) String)"

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine = error "Week07.JoinList#replaceLine not implemented for Buffer (JoinList (Score, Size) String)"

  numLines :: JoinList (Score, Size) String -> Int
  numLines = error "Week07.JoinList#numLines not implemented for Buffer (JoinList (Score, Size) String)"

  value :: JoinList (Score, Size) String -> Int
  value = error "Week07.JoinList#value not implemented for Buffer (JoinList (Score, Size) String)"

initialValue :: JoinList (Score, Size) String
initialValue = error "Week07.JoinList#initialValue not implemented"

main :: IO ()
main =
  runEditor editor initialValue
