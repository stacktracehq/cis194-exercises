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
parseMessage message = createMessage (words message)
  where
    createMessage :: [String] -> LogMessage
    createMessage ("I":t:c) = LogMessage (Info) (read t) (unwords c)
    createMessage ("W":t:c) = LogMessage (Warning) (read t) (unwords c)
    createMessage ("E":s:t:c) = LogMessage (Error(read s)) (read t) (unwords c)
    createMessage msg =  Unknown (unwords msg)



parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages) 


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert target Leaf =  Node Leaf target Leaf 
insert target@(LogMessage _ targetTime _) (Node left source@(LogMessage _ sourceTime _) right)
  | targetTime < sourceTime = Node (insert target left) source right
  | otherwise = Node left source (insert target right)
insert _ tree = tree


build :: [LogMessage] -> MessageTree
build msgList = foldr insert Leaf (reverse msgList)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = concat [inOrder(left), [msg], inOrder(right)]
inOrder _  = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong items = map giveMeContent (filterMe (inOrder (build items)))

   where 
    giveMeContent :: LogMessage -> String
    giveMeContent (LogMessage _ _ content) = content
    giveMeContent (Unknown content) = content

    filterMe :: [LogMessage] -> [LogMessage]
    filterMe input  = filter (\item -> case item of 
      (LogMessage (Error errorNum) _ _) ->  errorNum >= 50 
      _ -> False) input
