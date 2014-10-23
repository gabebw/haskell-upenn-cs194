{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Print out some messages, ordered by timestamp
main :: IO ()
main = do
  let numberOfMessages = 20
  messages <- testParse parse numberOfMessages "error.log"
  print $ inOrder (build messages)

parseMessage :: String -> LogMessage
parseMessage = parseMessagePieces . words

parseMessagePieces :: [String] -> LogMessage
parseMessagePieces ("E":errLevel:timestamp:body) = LogMessage (Error (read errLevel)) (read timestamp) (unwords body)
parseMessagePieces ("I":timestamp:body) = LogMessage Info (read timestamp) (unwords body)
parseMessagePieces ("W":timestamp:body) = LogMessage Warning (read timestamp) (unwords body)
parseMessagePieces body = Unknown (unwords body)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

hasGreaterTimestamp :: LogMessage -> LogMessage -> Bool
hasGreaterTimestamp (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 > ts2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m (Node ltree msg rtree)
  | hasGreaterTimestamp m msg = Node ltree msg (insert m rtree)
  | otherwise = Node (insert m ltree) msg rtree

build :: [LogMessage] -> MessageTree
-- Could also be: foldl (flip insert) Leaf
build = foldl (\tree m -> insert m tree) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m rtree) = m:(inOrder rtree)
inOrder (Node ltree m rtree) = inOrder ltree ++ [m] ++ inOrder rtree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong b = map message $ filter isSevere ordered
    where
        ordered = inOrder $ build b

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _) = level > 50
isSevere _ = False

message :: LogMessage -> String
message (LogMessage _ _ s) = s
