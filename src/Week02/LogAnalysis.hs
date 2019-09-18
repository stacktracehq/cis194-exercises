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
parseMessage line = case words line of
  "I"     : t : msg -> LogMessage Info (read t) (unwords msg)
  "W"     : t : msg -> LogMessage Warning (read t) (unwords msg)
  "E" : l : t : msg -> LogMessage (Error (read l)) (read t) (unwords msg)
  _                 -> Unknown line

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                       = tree
insert lm          Leaf                       = Node Leaf lm Leaf
insert lm          (Node lt u@(Unknown _) rt) = Node lt u (insert lm rt)
insert lm@(LogMessage _ t1 _) (Node lt y@(LogMessage _ t2 _) rt)
  | t1 <= t2  = Node (insert lm lt) y rt
  | otherwise = Node lt y (insert lm rt)

-- build :: [LogMessage] -> MessageTree
-- build = foldl (flip insert) Leaf

build :: [LogMessage] -> MessageTree
build []      = Leaf
build (h : t) = go t (insert h Leaf)
 where
  go :: [LogMessage] -> MessageTree -> MessageTree
  go []       tree = tree
  go (x : xs) tree = go xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf            = []
inOrder (Node lt lm rt) = inOrder lt ++ [lm] ++ inOrder rt

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong ls = map getMessage (inOrder (build (severeErrors ls)))
whatWentWrong = (getMessage <$>) . inOrder . build . severeErrors
 where
  severeErrors :: [LogMessage] -> [LogMessage]
  severeErrors logs = filter isSevereError logs

  isSevereError :: LogMessage -> Bool
  isSevereError (LogMessage (Error l) _ _) = l > 50
  isSevereError _                          = False

  getMessage :: LogMessage -> String
  getMessage (LogMessage _ _ m) = m
  getMessage (Unknown m       ) = m

