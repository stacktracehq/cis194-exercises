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
  , main
  ) where

import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..), (<>))
import Week07.Buffer (Buffer(..))
import Week07.Editor (editor, runEditor)
import Week07.Scrabble (Score(..), scoreString)
import Week07.Sized (Size(..), Sized(..), getSize)

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

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a = a
a +++ Empty = a
as +++ bs = Append (tag as <> tag bs) as bs

--------------------------- Exercise 2

instance Foldable (JoinList m) where
  foldr :: (a -> b -> b) -> b -> JoinList m a -> b
  foldr _ b Empty = b
  foldr f b (Single _ a) = f a b
  foldr f b (Append _ l r) = foldr f (foldr f b r) l

instance Sized b => Sized (JoinList b a) where
  size Empty = mempty
  size (Single m _) = size m
  size (Append m _ _) = size m

searchJ :: (Sized b, Monoid b) => (b -> Bool) -> JoinList b a -> Maybe a
searchJ p as
  | p (tag as) = go mempty as
  | otherwise = Nothing
  where
    go _ Empty = Nothing
    go _ (Single _ a) = Just a
    go i (Append _ l r)
      | p (i <> tag l) = go i l
      | otherwise = go (i <> tag l) r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i as
  | i < 0 = Nothing
  | otherwise = searchJ ((> Size i) . size) as

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n = go mempty
  where
    go _ Empty = Empty
    go i (Single m a)
      | Size n >= size (i <> m) = Empty
      | otherwise = Single m a
    go i (Append _ l r)
      | Size n >= size (i <> tag l) = go (i <> tag l) r
      | otherwise = go i l +++ go (i <> tag l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n = go mempty
  where
    go _ Empty = Empty
    go i (Single m a)
      | Size n < size (i <> m) = Empty
      | otherwise = Single m a
    go i (Append _ l r)
      | Size n < size (i <> tag l) = go i l
      | otherwise = go i l +++ go (i <> tag l) r

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine =
  Single <$> scoreString <*> id

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty = ""
  toString (Single _ a) = a
  toString (Append _ as bs) = toString as ++ toString bs

  fromString :: String -> JoinList (Score, Size) String
  fromString =
    foldr ((+++) . go) Empty . lines
      where
        go l = Single (scoreString l, Size 1) l

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line =
    indexJ

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine n _ as | Size n > size as = as
  replaceLine n str as =
    takeJ n as +++
    Single (scoreString str, Size 1) str +++
    dropJ (n + 1) as

  numLines :: JoinList (Score, Size) String -> Int
  numLines =
    getSize . snd . tag

  value :: JoinList (Score, Size) String -> Int
  value =
    getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue =
  fromString $
  unlines
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ]

main :: IO ()
main =
  runEditor editor initialValue
