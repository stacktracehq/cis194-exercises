module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  ) where

import Week08.Employee (GuestList(..), Employee(..))
import Data.Tree (Tree, foldTree)
import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import Data.Monoid ((<>))

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons = (<>) . (GL <$> pure <*> empFun)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = (glCons e . snd &&& fst) . mconcat

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree nextLevel

--------------------------- Exercise 5

main :: IO ()
main =
  do
    guests <- maxFun . read <$> readFile "../resources/Week08/company.txt"
    putStrLn $ "Total Fun: " ++ totalFun guests
    putStr $ empNames guests
  where
    totalFun = show . glFun
    empNames = unlines . map empName . glGuests
