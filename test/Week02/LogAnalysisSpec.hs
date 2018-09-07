module Week02.LogAnalysisSpec
  ( spec
  , hspec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Week02.LogAnalysis
  ( LogMessage(..)
  , MessageTree(..)
  , MessageType(..)
  , build
  , inOrder
  , insert
  , parse
  , parseMessage
  , whatWentWrong
  )

spec :: Spec
spec =
  describe "LogAnalysis" $ do
    parseMessageSpec
    parseSpec
    insertSpec
    buildSpec
    inOrderSpec
    whatWentWrongSpec

parseMessageSpec :: Spec
parseMessageSpec =
  describe "parseMessage" $
  it "converts a log line into a LogMessage" $ do
    parseMessage "E 2 562 help help"
      `shouldBe` LogMessage (Error 2) 562 "help help"
    parseMessage "I 29 la la la"
      `shouldBe` LogMessage Info 29 "la la la"
    parseMessage "W 45 A warning"
      `shouldBe` LogMessage Warning 45 "A warning"
    parseMessage "This is not in the right format"
      `shouldBe` Unknown "This is not in the right format"

parseSpec :: Spec
parseSpec =
  describe "parse" $ do
    it "converts many log lines into a list of log messages" $
      parse "E 2 562 help help\nI 29 la la la\nThis is not in the right format"
        `shouldBe`
          [ LogMessage (Error 2) 562 "help help"
          , LogMessage Info 29 "la la la"
          , Unknown "This is not in the right format"
          ]
    it "returns empty list for empty string" $
      parse "" `shouldBe` []
    it "returns singleton list for input without new lines" $
      parse "E 2 562 help help I 29 la la la"
        `shouldBe`
          [
            LogMessage (Error 2) 562 "help help I 29 la la la"
          ]

messageA :: LogMessage
messageA = LogMessage Info 1 "Nothing to report"

messageB :: LogMessage
messageB = LogMessage (Error 20) 2 "Too many pickles"

messageC :: LogMessage
messageC = LogMessage (Error 70) 3 "Way too many pickles"

messageD :: LogMessage
messageD = LogMessage Info 4 "Everything normal"

messageE :: LogMessage
messageE = LogMessage Warning 5 "Flange is due for a check-up"

messageF :: LogMessage
messageF = LogMessage Info 6 "Completed armadillo processing"

messageG :: LogMessage
messageG = LogMessage Info 7 "Out for lunch, back in two time steps"

messageH :: LogMessage
messageH = LogMessage (Error 65) 8 "Bad pickle-flange interaction detected"

messageI :: LogMessage
messageI = LogMessage Info 9 "Back from lunch"

messageJ :: LogMessage
messageJ = LogMessage (Error 99) 10 "Flange failed!"

messageK :: LogMessage
messageK = LogMessage Info 11 "Initiating self-destruct sequence"

unknown :: LogMessage
unknown = Unknown "Wat"

insertSpec :: Spec
insertSpec =
  describe "insert" $ do
    let tree = Node (Node Leaf messageB Leaf) messageD Leaf
    it "inserts a LogMessage into an empty tree, producing a single-node tree" $
      insert messageA Leaf `shouldBe` Node Leaf messageA Leaf
    it "inserts a new LogMessage into a sorted MessageTree, producing a new sorted MessageTree" $ do
      insert messageC tree `shouldBe`
        Node (Node Leaf messageB (Node Leaf messageC Leaf)) messageD Leaf
      insert messageA tree `shouldBe`
        Node (Node (Node Leaf messageA Leaf) messageB Leaf) messageD Leaf
    it "does not insert LogMessages of type 'Unknown'" $
      insert unknown tree `shouldBe` tree

buildSpec :: Spec
buildSpec =
  describe "build" $ do
    it "builds an empty tree when given no log messages" $
      build [] `shouldBe` Leaf
    it "builds a complete MessageTree from a list of unqique messages" $ do
      let inputOne =
            [ messageA
            , messageG
            , messageB
            ]
      let expectedOne = Node (Node Leaf messageA Leaf) messageB (Node Leaf messageG Leaf)
      build inputOne `shouldBe` expectedOne

      let inputTwo =
            [ messageC
            , messageA
            , messageD
            ]
      let expectedTwo = Node (Node Leaf messageA (Node Leaf messageC Leaf)) messageD Leaf
      build inputTwo`shouldBe` expectedTwo

inOrderSpec :: Spec
inOrderSpec =
  describe "inOrder" $ do
    it "returns an empty list when given an empty MessageTree" $
      inOrder Leaf `shouldBe` []
    it "returns an singlton list when given a single-node tree" $
      inOrder (Node Leaf messageA Leaf) `shouldBe` [messageA]
    it "takes a sorted MessageTree and produces a list of all the LogMessages it contains, sorted by timestamp from smallest to biggest" $ do
      let input =
            Node
              ( Node
                  ( Node
                      Leaf
                      messageA
                      Leaf
                  )
                  messageB
                  ( Node
                      Leaf
                      messageC
                      Leaf
                  )
              )
              messageD
              ( Node
                  ( Node
                      Leaf
                      messageE
                      Leaf
                  )
                  messageF
                  ( Node
                      Leaf
                      messageG
                      Leaf
                  )
              )
      let expected =
            [ messageA
            , messageB
            , messageC
            , messageD
            , messageE
            , messageF
            , messageG
            ]
      inOrder input `shouldBe` expected

whatWentWrongSpec :: Spec
whatWentWrongSpec =
  describe "whatWentWrong" $
    it "takes an unsorted list of LogMessage s, and returns a list of the messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp" $ do
      let input =
            [ messageF
            , messageA
            , messageJ
            , messageD
            , messageK
            , messageC
            , messageH
            , messageE
            , messageG
            , messageB
            , messageI
            ]
      let expected =
            [ "Way too many pickles"
            , "Bad pickle-flange interaction detected"
            , "Flange failed!"
            ]
      whatWentWrong input `shouldBe` expected
