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
insert lm Leaf = Node Leaf lm Leaf
insert lm tr@(Node left existing right)
  | isEarlier lm existing = Node (insert lm left) existing right
  | isLaterOrSameTime lm existing = Node left existing (insert lm right)
  | otherwise = tr

isEarlier :: LogMessage -> LogMessage -> Bool
isEarlier (LogMessage _ left _) (LogMessage _ right _) = left < right
isEarlier _ _ = False

isLaterOrSameTime :: LogMessage -> LogMessage -> Bool
isLaterOrSameTime (LogMessage _ left _) (LogMessage _ right _) = left >= right
isLaterOrSameTime _ _ = False

build :: [LogMessage] -> MessageTree
build = error "Week02.LogAnalysis#build not implemented"

inOrder :: MessageTree -> [LogMessage]
inOrder = error "Week02.LogAnalysis#inOrder not implemented"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = error "Week02.LogAnalysis#whatWentWrong not implemented"
