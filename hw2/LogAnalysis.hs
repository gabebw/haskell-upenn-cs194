{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseMessagePieces . words

parseMessagePieces :: [String] -> LogMessage
parseMessagePieces ("E":errLevel:timestamp:body) = LogMessage (Error (read errLevel)) (read timestamp) (unwords body)
parseMessagePieces ("I":timestamp:body) = LogMessage Info (read timestamp) (unwords body)
parseMessagePieces ("W":timestamp:body) = LogMessage Warning (read timestamp) (unwords body)
parseMessagePieces body = Unknown (unwords body)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines
