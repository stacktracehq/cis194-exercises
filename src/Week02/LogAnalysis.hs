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
                                                , Tree(..)
                                                , MessageTree
                                                , MessageType(..)
                                                , TimeStamp
                                                , testWhatWentWrong
                                                , testParse
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , runParser
                                                , takeRest
                                                )
import           Text.Megaparsec.Char           ( space1
                                                , char
                                                )

import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Applicative            ( (<|>) )
import           Data.Foldable                  ( foldl'
                                                , toList
                                                )

type Parser = Parsec Void String

messageParser :: Parser LogMessage
messageParser =
  let lexeme :: Parser a -> Parser a
      lexeme = L.lexeme (L.space space1 space1 space1)
      level :: Parser MessageType
      level =
          lexeme
            $   (Info <$ char 'I')
            <|> (Warning <$ char 'W')
            <|> (Error <$> (char 'E' *> space1 *> L.decimal))

      date :: Parser Int
      date = lexeme L.decimal
      msg :: Parser String
      msg = takeRest
  in  LogMessage <$> level <*> date <*> msg

parseMessage :: String -> LogMessage
parseMessage msg =
  let resultToLogMessage = either (const (Unknown msg)) id
  in  resultToLogMessage $ runParser messageParser "" msg

parse :: String -> [LogMessage]
parse = (parseMessage <$>) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert lm tree@(Node l lm' r) =
  let getTimeForMessage :: LogMessage -> Maybe TimeStamp
      getTimeForMessage (LogMessage _ t _) = Just t
      getTimeForMessage _                  = Nothing
  in  case (<) <$> getTimeForMessage lm <*> getTimeForMessage lm' of
        Nothing    -> tree
        Just True  -> Node (insert lm l) lm' r
        Just False -> Node l lm' (insert lm r)

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder = toList

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  let matchesSeverity :: (Int -> Bool) -> LogMessage -> Bool
      matchesSeverity f (LogMessage (Error s) _ _) = f s
      matchesSeverity _ _                          = False

      isSevere = matchesSeverity (>= 50)

      getMessage (LogMessage _ _ m) = m
      getMessage _                  = ""
  in  map getMessage . filter isSevere . inOrder . build
