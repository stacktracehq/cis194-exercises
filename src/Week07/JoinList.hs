{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Monoid ((<>))

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
(+++) a Empty = a
(+++) Empty b = b
(+++) a b = Append (tag a <> tag b) a b

--------------------------- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i t
  | i >= 0 && p (tag t) = go mempty t
  | otherwise = Nothing
  where
    p = (Size i <) . size
    go _ Empty = Nothing
    go _ (Single _ a) = Just a
    go s (Append _ l r)
      | p (s <> tag l) = go s l
      | otherwise = go (s <> tag l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n t
  | n == 0 = Empty
  | n > 0 && p (tag t) = go mempty t
  | otherwise = t
  where
    p = (Size n <=) . size
    go _ Empty = Empty
    go s (Append _ l r)
      | p (s <> tag l) = go s l
      | otherwise = l +++ go (s <> tag l) r
    go _ a = a

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n t
  | n == 0 = t
  | n > 0 && p (tag t) = go mempty t
  | otherwise = Empty
  where
    p = (Size n <) . size
    go _ Empty = Empty
    go s (Append _ l r)
      | p (s <> tag l) = go s l +++ r
      | otherwise = go (s <> tag l) r
    go _ a = a

-- replaceJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a -> JoinList b a
-- replaceJ i v t
--   | i >= 0 && p (tag t) = go mempty t
--   | otherwise = t
--   where
--     p = (Size i <) . size
--     go _ Empty = Empty
--     go _ (Single _ _) = v
--     go s (Append _ l r)
--       | p (s <> tag l) = go s l +++ r
--       | otherwise = l +++ go (s <> tag l) r

replaceJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a -> JoinList b a
replaceJ _ _ Empty = Empty
replaceJ i v t
  | i >= 0 && lessThanSize t = takeJ i t +++ v +++ dropJ (i + 1) t
  | otherwise = t
  where
    lessThanSize = (Size i <) . size . tag

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = Single =<< scoreString

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = concat . joinListToList

  fromString :: String -> JoinList (Score, Size) String
  fromString = foldr ((+++) . scoreLineWithSize) Empty . lines
    where
      scoreLineWithSize = Single =<< (, Size 1)  . scoreString

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine i = replaceJ i . fromString

  numLines :: JoinList (Score, Size) String -> Int
  numLines = getSize . snd . tag

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue = Empty

main :: IO ()
main =
  runEditor editor initialValue
