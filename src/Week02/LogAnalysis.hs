module Week02.LogAnalysis
  ( parseMessage
  , parse
  , insert
  , build
  , inOrder
  , whatWentWrong
  , module Week02.Log
  ) where

import Week02.Log
  ( LogMessage(..)
  , MessageTree(..)
  , MessageType(..)
  , TimeStamp
  , testWhatWentWrong
  , testParse
  )

parseMessage :: String -> LogMessage
parseMessage input =
  case words input of
    "I":time:content -> LogMessage Info (read time) (unwords content)
    "W":time:content -> LogMessage Warning (read time) (unwords content)
    "E":level:time:content ->
      LogMessage (Error (read level)) (read time) (unwords content)
    _ -> Unknown input

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ t _) = t
timeStamp (Unknown _) = 0

message :: LogMessage -> String
message (Unknown m) = m
message (LogMessage _ _ m) = m

severity :: LogMessage -> Int
severity (LogMessage (Error s) _ _) = s
severity _ = 0

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert Unknown {} t = t
insert a Leaf = Node Leaf a Leaf
insert a (Node l b r)
  | timeStamp a < timeStamp b = Node (insert a l) b r
  | otherwise = Node l b (insert a r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter ((> 50) . severity) . inOrder . build
