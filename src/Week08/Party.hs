module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  ) where

import Week08.Employee (GuestList(..), Employee(..))
import Data.Tree (Tree, foldTree)
import Data.List
import Data.Foldable

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp _ a) (GL xs b) = GL (x : xs) (a + b)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ a) y@(GL _ b) = if a >= b then x else y

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b l = (b `glCons` foldMap snd l, foldMap fst l)

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree nextLevel

--------------------------- Exercise 5

main :: IO ()
main = do 
  guestList <- maxFun . read <$> readFile "resources/Week08/company.txt"
  putStrLn $ "Total fun: " ++ show (glFun guestList) 
  traverse_ putStrLn $ sort $ empName <$> glGuests guestList
