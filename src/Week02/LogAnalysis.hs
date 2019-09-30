module Week02.LogAnalysis
  ( parseMessage
  , parse
  , insert
  , build
  , inOrder
  , whatWentWrong
  , module Week02.Log
  )
where

import           Week02.Log                     ( LogMessage(..)
                                                , MessageTree(..)
                                                , MessageType(..)
                                                , TimeStamp
                                                , testWhatWentWrong
                                                , testParse
                                                )

isInt :: String -> Bool
isInt s = not . null $ (reads s :: [(Int, String)])

pickType :: [String] -> LogMessage
pickType ("I" : time : message) | isInt time =
  LogMessage Info (read time) (unwords message)
pickType ("W" : time : message) | isInt time =
  LogMessage Warning (read time) (unwords message)
pickType ("E" : level : time : message) | isInt level && isInt time =
  LogMessage (Error (read level)) (read time) (unwords message)
pickType xs = Unknown . unwords $ xs

parseMessage :: String -> LogMessage
parseMessage = pickType . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt   = mt
insert lm          Leaf = Node Leaf lm Leaf
insert newLog@(LogMessage _ newTime _) (Node lt currentLog@(LogMessage _ currentTime _) rt)
  | newTime >= currentTime
  = Node lt currentLog (insert newLog rt)
  | otherwise
  = Node (insert newLog lt) currentLog rt
insert _ _ = error "Week02.LogAnalysis#insert encountered an unexpected state"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse
-- build = foldl (flip $ insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf             = []
inOrder (Node lT msg rT) = (inOrder lT) ++ [msg] ++ (inOrder rT)

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error s) _ _) = s > 50
isRelevant _                          = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(LogMessage _ _ m) -> m) . filter isRelevant . inOrder . build
