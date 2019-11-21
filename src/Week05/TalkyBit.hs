{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Week05.TalkyBit where

type PersonName = String
type TeamName = String
type ItemName = String
type Price = Double

data GroceryItem = GI ItemName Price deriving (Show, Read, Ord, Eq)

getPrice :: GroceryItem -> Double
getPrice (GI _ p) = p

------------------------------------- People

data Person = Person PersonName [GroceryItem]
  deriving (Show, Read, Ord, Eq)

getItems :: Person -> [GroceryItem]
getItems (Person _ is) = is

------------------------------------- Teams

data Team = Team TeamName [Person]
  deriving (Show, Read, Ord, Eq)

------------------------------------- Type class

------------------------------------- Type class instance for Person

------------------------------------- Type class instance for Team

------------------------------------- calcualteBill

-- we want to be able to calculate the bill for any type that
-- we can map to a list of grocery groceryItems. This mapping will
-- be different for different types, i.e. ad-hoc

calculateBill :: a -> Double
calculateBill = error "what to do?"

ben :: Person
ben = Person "Ben" [GI "Milk" 4, GI "Dreamy" 5, GI "Hotdog" 10]

krankie :: Person
krankie = Person
  "Krankie"
  [ GI "Milk"       4
  , GI "Milk"       4
  , GI "Milk"       4
  , GI "Paamaajaan" 7
  , GI "Paamaajaan" 7
  , GI "Paamaajaan" 7
  ]

team :: Team
team = Team "Kren" [ben, krankie]

benBill :: Double
benBill = calculateBill ben
krankieBill :: Double
krankieBill = calculateBill krankie
teamBill :: Double
teamBill = calculateBill team

result :: String
result =
  "\"Ben\": "
    <> show benBill
    <> ", Krankie: "
    <> show krankieBill
    <> ", Team: "
    <> show teamBill

