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
import           Week07.Scrabble                ( Score(..) )
import           Week07.Sized                   ( Sized(..)
                                                , Size(..)
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
tag = error "Week07.JoinList#tag not implemented"

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = error "Week07.JoinList#(+++) not implemented"

--------------------------- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = error "Week07.JoinList#indexJ not implemented"

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = error "Week07.JoinList#dropJ not implemented"

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = error "Week07.JoinList#takeJ not implemented"

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = error "Week07.JoinList#scoreLine not implemented"

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString =
    error
      "Week07.JoinList#toString not implemented for Buffer (JoinList (Score, Size) String)"

  fromString :: String -> JoinList (Score, Size) String
  fromString =
    error
      "Week07.JoinList#fromString not implemented for Buffer (JoinList (Score, Size) String)"

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line =
    error
      "Week07.JoinList#line not implemented for Buffer (JoinList (Score, Size) String)"

  replaceLine
    :: Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine =
    error
      "Week07.JoinList#replaceLine not implemented for Buffer (JoinList (Score, Size) String)"

  numLines :: JoinList (Score, Size) String -> Int
  numLines =
    error
      "Week07.JoinList#numLines not implemented for Buffer (JoinList (Score, Size) String)"

  value :: JoinList (Score, Size) String -> Int
  value =
    error
      "Week07.JoinList#value not implemented for Buffer (JoinList (Score, Size) String)"

initialValue :: JoinList (Score, Size) String
initialValue = error "Week07.JoinList#initialValue not implemented"

main :: IO ()
main = runEditor editor initialValue
