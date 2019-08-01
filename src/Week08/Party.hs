module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  )
where

import           Week08.Employee                ( GuestList(..)
                                                , Employee(..)
                                                )
import           Data.Tree                      ( Tree(..) )
import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( join )

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL { glGuests = currentGuests, glFun = currentFun }) =
  GL { glGuests = e : currentGuests, glFun = currentFun + empFun e }

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go where go (Node a as) = f a (go <$> as)

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = ((glCons e) . snd &&& fst) . mconcat

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

--------------------------- Exercise 5

printGuestList :: GuestList -> String
printGuestList g = "Test\n\n\n"

main :: IO String
main = join . (putStrLn . printGuestList . maxFun . read) <$> readFile
  "./resources/Week08/company.txt"
