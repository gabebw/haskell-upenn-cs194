{-# OPTIONS_GHC -Wall #-}
module LogAnalysis2 where

import Log

parseMessage :: String -> LogMessage
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
parseMessage = parseMessageWords . words

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I":timestamp:message) = LogMessage Info (read timestamp) (unwords message)
parseMessageWords ("W":timestamp:message) = LogMessage Warning (read timestamp) (unwords message)
parseMessageWords ("E":level:timestamp:message) = LogMessage (Error (read level)) (read timestamp) (unwords message)
parseMessageWords xs = Unknown (unwords xs)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert message tree =
