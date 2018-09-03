module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  ) where

import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Tree (Tree, foldTree)
import Week08.Employee (Employee(..), GuestList(..))

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons guest (GL guests funTotal) =
  GL (guest : guests) (funTotal + empFun guest)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree _is_ defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss = glCons boss . foldMap snd &&& foldMap fst

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . foldTree nextLevel

--------------------------- Exercise 5

totalFun :: GuestList -> String
totalFun = ("Total fun: " ++) . show . glFun

sortedNames :: GuestList -> [String]
sortedNames = sort . map empName . glGuests

main :: IO ()
main = do
  guests <- maxFun . read <$> readFile "./resources/Week08/company.txt"
  putStrLn $ totalFun guests
  mapM_ putStrLn $ sortedNames guests
