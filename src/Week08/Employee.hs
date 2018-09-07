{-# LANGUAGE InstanceSigs #-}

module Week08.Employee where

import Data.Tree (Tree(..))

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp
  { empName :: Name
  , empFun :: Fun
  } deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 2)
        [ Node (Emp "Joe" 5) [Node (Emp "John" 1) [], Node (Emp "Sue" 5) []]
        , Node (Emp "Fred" 3) []
        ]
    , Node (Emp "Sarah" 17) [Node (Emp "Sam" 4) []]
    ]

testCompany2 :: Tree Employee
testCompany2 =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 3) -- (8, 8)
        [ Node
            (Emp "Joe" 5) -- (5, 6)
            [ Node (Emp "John" 1) [] -- (1, 0)
            , Node (Emp "Sue" 5) [] -- (5, 0)
            ]
        , Node (Emp "Fred" 3) [] -- (3, 0)
        ]
    , Node
        (Emp "Sarah" 17) -- (17, 4)
        [ Node (Emp "Sam" 4) [] -- (4, 0)
        ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList =
  GL { glGuests :: [Employee], glFun :: Fun }
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

instance Semigroup GuestList where
  (<>) :: GuestList -> GuestList -> GuestList
  (<>) = error "Week08.Party#Monoid(GuestList)#mappend not implemented"

instance Monoid GuestList where
  mempty :: GuestList
  mempty = GL [] 0

  mappend :: GuestList -> GuestList -> GuestList
  mappend (GL xs a) (GL ys b) = GL (xs ++ ys) (a + b)
