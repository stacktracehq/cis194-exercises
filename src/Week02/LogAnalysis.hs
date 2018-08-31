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

parseMessageParts :: [String] -> LogMessage
parseMessageParts ("I" : t : ms) = LogMessage Info (read t) (unwords ms)
parseMessageParts ("W" : t : ms) = LogMessage Warning (read t) (unwords ms)
parseMessageParts ("E" : s : t : ms) = LogMessage (Error (read s)) (read t) (unwords ms)
parseMessageParts ms = Unknown (unwords ms)

isBefore :: LogMessage -> LogMessage -> Bool
isBefore (LogMessage _ a _) (LogMessage _ b _) = a < b
isBefore _ _ = True

matchesSeverity :: (Int -> Bool) -> LogMessage -> Bool
matchesSeverity f (LogMessage (Error s) _ _) = f s
matchesSeverity _ _ = False

parseMessage :: String -> LogMessage
parseMessage = parseMessageParts . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert nm Leaf = Node Leaf nm Leaf
insert nm (Node l m r) =
  if isBefore nm m
    then Node (insert nm l) m r
    else Node l m (insert nm r)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r)  = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ m) -> m) . filter (matchesSeverity (>= 50)) . inOrder . build
