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
parseMessage = error "Week02.LogAnalysis#parseMessage not implemented"

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
