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

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords ("I":t:rest) = LogMessage Info (read t) (unwords rest)
parseWords ("W":t:rest) = LogMessage Warning (read t) (unwords rest)
parseWords ("E":n:t:rest) = LogMessage (Error (read n)) (read t) (unwords rest)
parseWords x = Unknown . unwords $ x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert n Leaf = Node Leaf n Leaf
insert n@(LogMessage _ nt _) (Node l m@(LogMessage _ mt _) r)
  | nt < mt = Node (insert n l) m r
  | otherwise = Node l m (insert n r)
insert _ mt = mt

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder = error "Week02.LogAnalysis#inOrder not implemented"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = error "Week02.LogAnalysis#whatWentWrong not implemented"
