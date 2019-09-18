module TalkyBit where

data GroceryItem =
  Milk
    | Dreamy
    | Jam deriving (Show, Eq, Ord, Read)

data GroceryRequest =
  SingleItemRequest GroceryItem
    | BulkItemRequest Int GroceryItem
    | Unknown deriving (Show, Eq, Ord, Read)

data GroceryList =
  Empty
    | (:|) GroceryRequest GroceryList deriving (Show, Eq, Ord, Read)

mapGl :: (GroceryRequest -> a) -> GroceryList -> [a]
mapGl _ Empty         = []
mapGl f (req :| list) = (f req) : mapGl f list

lookupPrice :: GroceryItem -> Double
lookupPrice Milk   = 4.5
lookupPrice Dreamy = 5
lookupPrice Jam    = 15

acceptRequest :: String -> GroceryRequest
acceptRequest s = case words s of
  ["Milk"  ]    -> SingleItemRequest Milk
  ["Dreamy"]    -> SingleItemRequest Dreamy
  ["Jam"   ]    -> SingleItemRequest Jam
  ["Milk"  , i] -> BulkItemRequest (read i) Milk
  ["Dreamy", i] -> BulkItemRequest (read i) Dreamy
  ["Jam"   , i] -> BulkItemRequest (read i) Jam
  _             -> Unknown

groceryBill :: GroceryList -> Double
groceryBill list = sum $ mapGl calc list
 where
  calc :: GroceryRequest -> Double
  calc (SingleItemRequest item  ) = lookupPrice item
  calc (BulkItemRequest qty item) = fromIntegral qty * lookupPrice item
  calc Unknown                    = 0

