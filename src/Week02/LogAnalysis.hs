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
parseMessage line = case words line of
  "E" : severity : timeStamp : message -> LogMessage (Error (read severity)) (read timeStamp) (unwords message)
  "W" : timeStamp : message -> LogMessage Warning (read timeStamp) (unwords message)
  "I" : timeStamp : message -> LogMessage Info (read timeStamp) (unwords message)
  _ -> Unknown line

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message @ (LogMessage _ timeStamp _) (Node left nodeMessage @ (LogMessage _ nodeTimeStamp _) right)
  | timeStamp < nodeTimeStamp = Node (insert message left) nodeMessage right
  | otherwise = Node left nodeMessage (insert message right)
insert LogMessage{} (Node _ (Unknown _) _) = error "Not possible"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

isImportant :: LogMessage -> Bool
isImportant (LogMessage (Error severity) _ _)
  | severity >= 50 = True
isImportant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ message) -> message) . filter isImportant . inOrder . build
