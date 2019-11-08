{-# LANGUAGE NoImplicitPrelude #-}

module TalkyBit where

import           Prelude                 hiding ( (.) )
import           Data.List                      ( foldl' )

newtype Name = Name String deriving (Show, Eq)

names :: [Name]
names = [Name "Fi", Name "Krankie", Name "Wynand"]

isAKrankie :: Name -> Bool
isAKrankie = \n -> n == Name "Krankie"
-- isAKrankie (Name "Krankie") = True
-- isAKrankie _                = False

filterKrankies :: [Name] -> [Name]
filterKrankies = filter (== Name "Krankie")

add :: Int -> (Int -> (Int -> (Int -> Int)))
add a b c d = a + b + c + d

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g a = f (g a)

tooManyKrankies :: [String] -> Bool
tooManyKrankies = (> 1) . length . filter isAKrankie . map Name

----------------------------------------- Folds
-- we've seen this pattern a bit by now

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

sumF :: Num a => [a] -> a
sumF = foldR (+) 0

product' :: Num a => [a] -> a
product' []       = 1
product' (x : xs) = x * product' xs

productF :: Num a => [a] -> a
productF = foldR (*) 1

length' :: [a] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs

lengthF :: [a] -> Int
lengthF = foldR (const (+ 1)) 0

anyTrueR :: [Bool] -> Bool
anyTrueR = foldr (||) False

anyTrueL :: [Bool] -> Bool
anyTrueL = foldl' (||) False

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR _ b []       = b
foldR f b (x : xs) = f x (foldR f b xs)

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL _ b []       = b
foldL f b (x : xs) = foldL f (f b x) xs

