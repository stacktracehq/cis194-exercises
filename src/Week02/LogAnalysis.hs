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

isInt :: String -> Bool
isInt s = not $ null (reads s :: [(Int, String)])

parseInt :: String -> Int
parseInt = read

parseMessageParts :: [String] -> LogMessage
parseMessageParts ("I":time:rest) | isInt time = 
  LogMessage Info (parseInt time) (unwords rest) 
parseMessageParts ("W":time:rest) | isInt time = 
  LogMessage Warning (parseInt time) (unwords rest) 
parseMessageParts ("E":level:time:rest) | isInt level && isInt time = 
  LogMessage (Error (parseInt level)) (parseInt time) (unwords rest) 
parseMessageParts x = Unknown (unwords x) 

parseMessage :: String -> LogMessage
parseMessage = parseMessageParts . words

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node lT nodeMsg@(LogMessage _ nodeTime _) rT) 
  | msgTime < nodeTime = Node (insert msg lT) nodeMsg rT
  | otherwise          = Node lT nodeMsg (insert msg rT)
insert _ (Node _ (Unknown _ ) _) = error "Unexpected MessageTree state"

build :: [LogMessage] -> MessageTree
build = foldl (flip $ insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lT msg rT) = (inOrder lT) ++ [msg] ++ (inOrder rT) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . (filter isError) . inOrder . build 
  where 
    isError :: LogMessage -> Bool
    isError (LogMessage (Error s) _ _)
      | s >= 50 = True
      | otherwise = False
    isError _ = False

    getMessage :: LogMessage -> String
    getMessage (LogMessage _ _ msg) = msg
    getMessage (Unknown _) = error "Unexpected LogMessage state"
