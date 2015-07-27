{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List (sortBy, isInfixOf)

-- Exercise 1

-- parseMessage "E 2 562 help help" == ValidLM (LogMessage (Error 2) 562 "help help")
-- parseMessage "I 29 la la la" == ValidLM (LogMessage Info 29 "la la la")
parseMessage :: String -> MaybeLogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> MaybeLogMessage
parseMessage' ("E":level:time:xs) = ValidLM (LogMessage (Error (read level)) (read time) (unwords xs))
parseMessage' ("I":time:xs) = ValidLM (LogMessage Info (read time) (unwords xs))
parseMessage' ("W":time:xs) = ValidLM (LogMessage Warning (read time) (unwords xs))
parseMessage' xs = InvalidLM (unwords xs)

-- Exercise 2

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly xs = map getLogMessage $ filter isValid xs
    where
        getLogMessage (ValidLM lm) = lm
        getLogMessage _ = error "They should all be valid"

isValid :: MaybeLogMessage -> Bool
isValid (InvalidLM _) = False
isValid _ = True

-- Exercise 3

parse :: String -> [LogMessage]
parse = validMessagesOnly . (map parseMessage) . lines

-- Exercise 4

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 `compare` ts2

-- Exercise 5

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- Exercise 6
-- Try:
-- > testWhatWentWrong parse whatWentWrong "error.log"
-- > testWhatWentWrong parse whatWentWrong "sample.log"

-- Takes an unsorted list of LogMessages, and returns a list of the messages
-- corresponding to any errors with a severity of 50 or greater, sorted by
-- timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = (map message) $ sortMessages $ (filter isSevereError) xs

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity > 50
isSevereError _ = False

message :: LogMessage -> String
message (LogMessage _ _ s) = s

-- Exercise 7

messagesAbout :: String -> [LogMessage] -> [String]
messagesAbout word = map message . filter (containsWord word)

containsWord :: String -> LogMessage -> Bool
containsWord w (LogMessage _ _ s) = w `isInfixOf` s

-- Exercise 8

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced w lms = whatWentWrong lms ++ messagesAbout w lms

-- Alternate solution:
-- whatWentWrongEnhanced w = (map message) . filter (isSevereError ||| containsWord w)
--
-- (|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
-- (|||) f g x = f x || g x
