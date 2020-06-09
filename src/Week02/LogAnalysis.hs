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
parseMessageWords (s:ss)
  | s == "E" = case parseError ss of
      Nothing -> Unknown (unwords (s:ss))
      Just logMessage -> logMessage      
  | s == "I" = case parseTimestamp Info ss of
      Nothing -> Unknown (unwords (s:ss))
      Just logMessage -> logMessage
  | s == "W" = case parseTimestamp Warning ss of
      Nothing -> Unknown (unwords (s:ss))
      Just logMessage -> logMessage
  | otherwise = Unknown (unwords (s:ss))

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
parse = error "Week02.LogAnalysis#parse not implemented"

insert :: LogMessage -> MessageTree -> MessageTree
insert = error "Week02.LogAnalysis#insert not implemented"

build :: [LogMessage] -> MessageTree
build = error "Week02.LogAnalysis#build not implemented"

inOrder :: MessageTree -> [LogMessage]
inOrder = error "Week02.LogAnalysis#inOrder not implemented"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = error "Week02.LogAnalysis#whatWentWrong not implemented"
