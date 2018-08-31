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
import Week07.Scrabble (Score(..), scoreString, getScore)
import Week07.Sized (Sized(..), Size(..), getSize)

import Data.Monoid ()

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
(+++) Empty b = b
(+++) a Empty = a
(+++) a b = Append (tag a <> tag b) a b

--------------------------- Exercise 2

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ v)
    | i == 0 = Just v
    | otherwise = Nothing
indexJ i (Append _ l r)
    | i < ln = indexJ i l
    | i - ln < rn = indexJ (i - ln) r
    | otherwise = Nothing
    where 
      ln = tagSize l
      rn = tagSize r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n x@(Single _ _)
    | n > 0 = Empty
    | otherwise = x
dropJ n x@(Append _ l r)
    | n >= ln + rn = Empty
    | n >= ln = dropJ (n - ln) r
    | n > 0 = dropJ n l +++ r
    | otherwise = x
    where 
      ln = tagSize l
      rn = tagSize r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n x@(Single _ _)
    | n > 0 = x
    | otherwise = Empty
takeJ n x@(Append _ l r)
    | n >= ln + rn = x
    | n >= ln = l +++ takeJ (n - ln) r
    | n > 0 = takeJ n l
    | otherwise = Empty
    where 
      ln = tagSize l
      rn = tagSize r

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = flip Single <*> scoreString

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = mconcat . joinListToList

  fromString :: String -> JoinList (Score, Size) String
  fromString s = foldr addLine Empty (lines s)
    where addLine l list = Single (scoreString l, 1) l +++ list 

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine n _ list | n < 0 || n > tagSize list = list
  replaceLine n l list = takeJ n list +++ fromString l +++ dropJ (n + 1) list

  numLines :: JoinList (Score, Size) String -> Int
  numLines = tagSize

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue = fromString "hello!"

main :: IO ()
main =
  runEditor editor initialValue
