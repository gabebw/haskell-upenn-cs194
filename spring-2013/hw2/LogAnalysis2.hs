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

-- Exercise 2: insert a single message into a MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert _ (Node _ (Unknown _) _) = error "stop it"
insert message@(LogMessage _ timestamp1 _) (Node left nodeMessage@(LogMessage _ timestamp2 _) right)
    | timestamp1 <= timestamp2 = Node (insert message left) nodeMessage right
    | otherwise = Node left nodeMessage (insert message right)

-- Exercise 3: Build a MessageTree from a list of LogMessages
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Or, as an explicit fold without `foldl`:
-- build ms = build' Leaf ms
-- build' tree [] = tree
-- build' tree (m:ms) = build' (insert m tree) ms

-- Exercise 4: produce sorted list of LogMessages by traversing the MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree message rtree) = inOrder ltree ++ [message] ++ inOrder rtree

-- Exercise 5: Take an _unsorted_ list of LogMessages, and return a list of the
-- messages corresponding to errors with severity >= 50, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map message) . (filter isSevereError) . inOrder . build
    where
        isSevereError :: LogMessage -> Bool
        isSevereError (LogMessage (Error level) _ _) = level >= 50
        isSevereError _ = False

        message (LogMessage _ _ text) = text
