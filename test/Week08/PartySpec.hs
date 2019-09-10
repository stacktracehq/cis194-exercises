module Week08.PartySpec
  ( spec
  , hspec
  )
where

import           Data.Tree                      ( Tree(..) )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Week08.Employee                ( Employee(..)
                                                , Fun
                                                , GuestList(..)
                                                )
import           Week08.Party                   ( glCons
                                                , maxFun
                                                , moreFun
                                                , nextLevel
                                                )

spec :: Spec
spec = describe "Week 08" $ do
  glConsSpec
  guestListMonoidSpec
  moreFunSpec
  nextLevelSpec
  maxFunSpec
  companyFileSpec

bparker, bbow, mfurness, btefay, koleary :: Employee
fferraioli, jottaway, dsowter, chorwill :: Employee
bparker = Emp "Brad Parker" 23
bbow = Emp "Brad Bow" 1
mfurness = Emp "Matt Furness" 56
btefay = Emp "Ben Tefay" 110
koleary = Emp "Keiran O'Leary" 87
fferraioli = Emp "Frankie Ferrailoli" 77
jottaway = Emp "James Ottaway" 65
dsowter = Emp "Dan Sowter" 319
chorwill = Emp "Chris Horwill" 43

toGL :: [Employee] -> GuestList
toGL es = GL es (sum (map empFun es))

founders, teamLeads, engineers, brads, empty :: GuestList
founders = toGL [dsowter, chorwill]
teamLeads = toGL [jottaway]
engineers = toGL [bparker, bbow, mfurness, btefay, koleary, fferraioli]
brads = toGL [bparker, bbow]
empty = GL [] 0

singleton :: Employee -> GuestList
singleton e = GL [e] (empFun e)

fromList :: [Employee] -> GuestList
fromList = foldr glCons empty

glConsSpec :: Spec
glConsSpec = describe "glCons" $ do
  it "adding element to empty list"
    $          glCons bparker empty
    `shouldBe` singleton bparker
  it "adding element to non-empty list" $ do
    glCons bbow (GL [bparker] (empFun bparker))
      `shouldBe` GL [bbow, bparker] (empFun bparker + empFun bbow)
    glCons btefay brads
      `shouldBe` GL (btefay : glGuests brads) (empFun btefay + glFun brads)
    glCons mfurness founders
      `shouldBe` GL
                   (mfurness : glGuests founders)
                   (empFun mfurness + glFun founders)

guestListMonoidSpec :: Spec
guestListMonoidSpec = describe "instance Monoid GuestList" $ do
  it "satisfies the right identity law" $ do
    empty <> mempty `shouldBe` empty
    engineers <> mempty `shouldBe` engineers
  it "satisfies the left identity law" $ do
    mempty <> empty `shouldBe` empty
    mempty <> founders `shouldBe` founders
  it "satisifed the associativity law" $ do
    (brads <> empty) <> founders `shouldBe` brads <> (empty <> founders)
    (brads <> founders) <> teamLeads `shouldBe` brads <> (founders <> teamLeads)

moreFunSpec :: Spec
moreFunSpec = describe "moreFun" $ it "chooses funnest list" $ do
  moreFun empty brads `shouldBe` brads
  moreFun brads empty `shouldBe` brads
  moreFun brads founders `shouldBe` founders
  moreFun engineers teamLeads `shouldBe` engineers

nextLevelSpec :: Spec
nextLevelSpec = describe "nextLevel" $ do
  it "employee has no subtrees"
    $          nextLevel mfurness []
    `shouldBe` (singleton mfurness, empty)
  it "employee has single subtree" $ do
    nextLevel fferraioli [(singleton bbow, singleton jottaway)] -- 77 [(1, 65)]
      `shouldBe` (glCons fferraioli (singleton jottaway), singleton bbow) -- I don't understand why snd isn't singlton jottaway...
    nextLevel jottaway [(engineers, brads)]
      `shouldBe` (glCons jottaway brads, engineers)
  it "employee has many subtrees"
    $          nextLevel
                 bbow
                 [ (singleton mfurness, singleton jottaway)
                 , (singleton chorwill, singleton btefay)
                 , (singleton bparker , singleton fferraioli)
                 ]
    `shouldBe` ( glCons
                 bbow
                 (singleton jottaway <> singleton btefay <> singleton fferraioli
                 )
               , singleton mfurness <> singleton chorwill <> singleton bparker
               )

stOne :: Tree Employee
stOne = Node dsowter [Node bbow [Node btefay [], Node bparker []]]

stTwo :: Tree Employee
stTwo = Node
  chorwill
  [ Node jottaway [Node fferraioli []]
  , Node koleary  [Node bparker [], Node bbow []]
  , Node mfurness []
  ]

testCompany :: Tree Employee
testCompany = Node
  (Emp "Stan" 9) -- (9+1+5+3+4=22,2+1+5+17=25)
  [ Node
    (Emp "Bob" 2) -- (2+1+5=8,5+3=8)
    [ Node
      (Emp "Joe" 5) -- (5,1+5=6)
      [ Node (Emp "John" 1) []
      , -- (1,0)
        Node (Emp "Sue" 5)  [] -- (5,0)
      ]
    , Node (Emp "Fred" 3) [] -- (3,0)
    ]
  , Node (Emp "Sarah" 17) -- (17,4)
         [Node (Emp "Sam" 4) [] -- (4,0)
                               ]
  ]

maxFunSpec :: Spec
maxFunSpec = describe "maxFunSpec" $ do
  it "single node tree" $ maxFun (Node dsowter []) `shouldBe` singleton dsowter
  it "multi-node tree" $ do
    maxFun stOne `shouldBe` fromList [dsowter, btefay, bparker]
    maxFun stTwo `shouldBe` fromList [jottaway, koleary, mfurness]
    maxFun testCompany `shouldBe` fromList
      [Emp "Bob" 2, Emp "John" 1, Emp "Sue" 5, Emp "Sarah" 17]

runIO :: IO (Fun, Int)
runIO = do
  guests <- maxFun . read <$> readFile "./resources/Week08/company.txt"
  return (glFun guests, length (glGuests guests))

companyFileSpec :: Spec
companyFileSpec =
  describe "evaluating company.txt"
    $          it "should have 458 guests, with a fun total of 21669"
    $          unsafePerformIO runIO
    `shouldBe` (21669, 458)
