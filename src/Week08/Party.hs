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
import           Data.Tree                      ( Tree )

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons = error "Week08.Party#glCons not implemented"

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = error "Week08.Party#moreFun not implemented"

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = error "Week08.Party#nextLevel not implemented"

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = error "Week08.Party#maxFun not implemented"

--------------------------- Exercise 5

main :: IO ()
main = putStrLn "Do the thing"
