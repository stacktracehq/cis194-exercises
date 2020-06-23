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
import Text.Read

parseMessage :: String -> LogMessage
parseMessage str = parseMessageWords (words str)

parseMessageWords :: [String] -> LogMessage
parseMessageWords [] = Unknown ""
parseMessageWords (s:ss) = case (getParser s) ss of
  Nothing -> Unknown (unwords (s:ss))
  Just logMessage -> logMessage  

getParser :: String -> [String] -> Maybe LogMessage
getParser "E" = parseError
getParser "I" = parseTimestamp Info
getParser "W" = parseTimestamp Warning
getParser _ = nothingFromArray

nothingFromArray :: [String] -> Maybe LogMessage
nothingFromArray _ = Nothing

parseError :: [String] -> Maybe LogMessage
parseError [] = Nothing
parseError (s:ss) = case readMaybe s of
  Nothing -> Nothing
  Just errorLevel -> parseTimestamp (Error errorLevel) ss

parseTimestamp :: MessageType -> [String] -> Maybe LogMessage
parseTimestamp _ [] = Nothing
parseTimestamp mt (s:ss) = case readMaybe s of
  Nothing -> Nothing
  Just timestamp -> Just (LogMessage mt timestamp (unwords ss))

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node left existing right)
  | isEarlier lm existing = Node (insert lm left) existing right
  | otherwise = Node left existing (insert lm right)

isEarlier :: LogMessage -> LogMessage -> Bool
isEarlier (LogMessage _ left _) (LogMessage _ right _) = left < right
isEarlier _ _ = False

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm (build (reverse lms))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node Leaf lm right) = [lm] ++ inOrder right
inOrder (Node left lm Leaf) = inOrder left ++ [lm]
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = [getMessage lm | lm <- inOrder (build lms), isRelevant lm]

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error lvl) _ _) = lvl >= 50
isRelevant _ = False
