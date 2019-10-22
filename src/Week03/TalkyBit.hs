{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFoldable #-}

module TalkyBit where

import           Prelude                        ( Show
                                                , Eq
                                                , Ord
                                                , Read
                                                , Double
                                                , String
                                                , Int
                                                , Bool
                                                , Foldable
                                                , abs
                                                , even
                                                , fromIntegral
                                                , const
                                                , otherwise
                                                , foldr
                                                , foldl
                                                , show
                                                , sum
                                                , undefined
                                                , error
                                                , (+)
                                                , (++)
                                                )

----------------- A recursive list type, specialised to ints

data IntList = Empty |  Int :| IntList
  deriving Show

-- (a & b) & c == a & (b & c)

-- a & b & c == a & (b & c) != (a & b) & cjj
-- explain this a little bit
infixr 7 :|

intList :: IntList
intList = (-1) :| 3 :| 4 :| Empty

----------------- Manually recursing is soooo Week 1

-- mapping an operation
absAll :: IntList -> IntList
absAll Empty     = Empty
absAll (x :| xs) = (abs x) :| (absAll xs)

addOne :: IntList -> IntList
addOne Empty     = Empty
addOne (x :| xs) = (x + 1) :| (addOne xs)

mapInts :: (Int -> Int) -> IntList -> IntList
mapInts _ Empty     = Empty
mapInts f (x :| xs) = f x :| mapInts f xs

-- filtering according to a predicate
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (x :| xs) | even x    = x :| (keepOnlyEven xs)
                       | otherwise = keepOnlyEven xs


filterInts :: (Int -> Bool) -> IntList -> IntList
filterInts _ Empty = Empty
filterInts f (x :| xs) | f x       = x :| (filterInts f xs)
                       | otherwise = filterInts f xs

data List a = E | a :|| List a
  deriving (Show, Foldable)

list :: List Int
list = (-1) :|| 3 :|| 4 :|| E

infixr 7 :||

(<$>) :: (a -> b) -> List a -> List b
(<$>) _ E          = E
(<$>) f (x :|| xs) = f x :|| f <$> xs

head :: List a -> Maybe a
head (x :|| _) = Just x
head E         = Nothing

data Maybe a = Nothing | Just a deriving (Show)

data NEL a = NEL a (List a) deriving (Show)

headNel :: NEL a -> a
headNel (NEL a _) = a
