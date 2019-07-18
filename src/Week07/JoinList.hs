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
  )
where

import           Week07.Buffer                  ( Buffer(..) )
import           Week07.Editor                  ( editor
                                                , runEditor
                                                )
import           Week07.Scrabble                ( Score(..)
                                                , scoreString
                                                )
import           Week07.Sized                   ( Sized(..)
                                                , Size(..)
                                                , getSize
                                                )

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

joinListToList :: JoinList m a -> [a]
joinListToList Empty          = []
joinListToList (Single _ a  ) = [a]
joinListToList (Append _ l r) = joinListToList l ++ joinListToList r

(!!?) :: Int -> [a] -> Maybe a
(!!?) n = lookup n . zip [0 ..]

--------------------------- Exercise 1

-- Suggestion (no tests):
-- Pulls the monoidal value out of the root of the JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

--------------------------- Exercise 2

joinListSize :: (Sized b, Monoid b) => JoinList b a -> Int
joinListSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a) | n == 0    = Just a
                      | otherwise = Nothing
indexJ n (Append _ l r) | joinListSize l > n = indexJ n l
                        | otherwise          = indexJ (n - joinListSize l) r


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _) | n == 0    = jl
                        | otherwise = Empty
dropJ n (Append _ l r) | joinListSize l > n = (dropJ n l) +++ r
                       | otherwise           = dropJ (n - joinListSize l) r


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _) | n <= 0    = Empty
                        | otherwise = jl
takeJ n (Append _ l r) | joinListSize l >= n = takeJ n l
                       | otherwise = l +++ takeJ (n - joinListSize l) r

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--------------------------- Exercise 4

pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x : y : t) = f x y : pairs f t
pairs _ t           = t

foldt :: (a -> a -> a) -> a -> [a] -> a
foldt _ z []  = z
foldt _ _ [x] = x                             -- aka foldt' of data-ordlist
foldt f z xs  = foldt f z (pairs f xs)

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty          = ""
  toString (Single _ s  ) = s
  toString (Append _ l r) = toString l ++ toString r

  fromString :: String -> JoinList (Score, Size) String
  fromString = (foldt (+++) Empty) . (toSingle <$>) . lines   where
    toSingle :: String -> JoinList (Score, Size) String
    toSingle s = Single (scoreString s, Size 1) s

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine
    :: Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine n s l
    | joinListSize l > n = replaceLine' (takeJ (n) l)
                                        (fromString s)
                                        (dropJ (n + 1) l)
    | otherwise = l
   where
    replaceLine'
      :: JoinList (Score, Size) String
      -> JoinList (Score, Size) String
      -> JoinList (Score, Size) String
      -> JoinList (Score, Size) String
    replaceLine' Empty y Empty = y
    replaceLine' x     y Empty = x +++ y
    replaceLine' Empty y z     = y +++ z
    replaceLine' x     y z     = x +++ y +++ z

  numLines :: JoinList (Score, Size) String -> Int
  numLines = joinListSize

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue =
  fromString
    $  "This buffer is for notes you don't want to save, and for\n"
    ++ "evaluation of steam valve coefficients.\n"
    ++ "To load a different file, type the character L followed\n"
    ++ "by the name of the file.\n"

main :: IO ()
main = runEditor editor initialValue
