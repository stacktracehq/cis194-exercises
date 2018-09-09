module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  ) where

import Week08.Employee (GuestList(..), Employee(..))
import Data.Tree (Tree, foldTree)
import Data.List (sort)

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun = existingFun} (GL es fun) = GL (e:es) (existingFun + fun)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e subs = (glCons e (mconcat (snd <$> subs)), mconcat (fst <$> subs))

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree nextLevel

--------------------------- Exercise 5

main :: IO ()
main = do
  content <- readFile "resources/Week08/company.txt"
  let funGuestList = maxFun $ read content
  let fun = glFun funGuestList
  let sortedEmpoyees = sort (empName <$> glGuests funGuestList)
  putStrLn ("Total fun: " ++ show fun)
  mconcat (putStrLn <$> sortedEmpoyees)
